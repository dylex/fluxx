import Control.Arrow (Kleisli(..), (***))
import Control.Exception (onException)
import Control.Monad ((<=<), when, unless, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, withExceptT, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks, local)
import qualified Data.Aeson as JS
import qualified Data.Aeson.Encode as JSE
import qualified Data.Aeson.Types as JS
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isAlphaNum)
import Data.Foldable (fold)
import Data.Function (fix)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as HCT
import qualified Network.HTTP.Types as H
import qualified Network.URI as URI
import qualified System.Console.GetOpt as Opt
import qualified System.Console.Haskeline as IL
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing)
import System.Environment (getProgName, getArgs)
import System.Exit (exitFailure)
import qualified System.FilePath as FP
import System.IO (stderr, hPutStrLn, withFile, IOMode(WriteMode))
import System.Process (callProcess)
import qualified Text.HTML.TagSoup as HTS
import qualified Text.HTML.TagSoup.Tree as HTS

titleUUID, attachmentUUID :: UUID.UUID
titleUUID = read "cf887ff0-faab-467c-8786-a41e8ac4d9ea"
attachmentUUID = read "d08507ce-1503-4bdc-d696-6c9728ec747c"

data State = State
  { stateTime :: UTCTime
  , stateCache :: FilePath
  , stateHTTP :: HC.Manager
  , stateRequest :: HC.Request
  , stateOutput :: FilePath
  }

newtype RunM a = RunM { runM :: ExceptT String (ReaderT State IO) a }
  deriving (Functor, Applicative, MonadReader State, MonadError String, MonadIO)

instance Monad RunM where
  RunM m >>= f = RunM $ m >>= runM . f
  RunM m >> RunM f = RunM $ m >> f
  return = RunM . return
  fail = throwError

runIn :: String -> FilePath -> RunM a -> RunM a
runIn t f (RunM m) = RunM $ withExceptT (("In " ++ t ++ ' ' : f) ++) $ do
  liftIO . createDirectoryIfMissing False =<< asks stateOutput
  local (\s -> s{ stateOutput = stateOutput s FP.</> f' }) $ do
    liftIO . putStrLn =<< asks stateOutput
    m
  where
  (n, e) = FP.splitExtension $ filter (not . (`elem` ' ':FP.pathSeparators)) f
  f' = FP.makeValid $ FP.addExtension (take (255 - length e) n) e

httpRequestJSON :: JS.FromJSON a => HC.Request -> RunM a
httpRequestJSON q = do
  m <- asks stateHTTP
  liftIO $ HC.withResponse q{ HC.requestHeaders = (H.hAccept, "application/json") : HC.requestHeaders q } m
    (\r -> either (fail . (++) "parsing response: ") return . (JS.parseEither JS.parseJSON <=< AP.eitherResult)
      =<< AP.parseWith (HC.responseBody r) JS.json BS.empty)

httpRequestDownload :: HC.Request -> RunM ()
httpRequestDownload q = do
  f <- asks stateOutput
  m <- asks stateHTTP
  liftIO $ HC.withResponse q m $ \r ->
    withFile f WriteMode $ \h -> fix $ \loop -> do
      b <- HC.brRead $ HC.responseBody r
      unless (BS.null b) $
        BS.hPutStr h b >> loop

newtype TextJSON = TextJSON { unTextJSON :: JS.Value }

instance JS.ToJSON TextJSON where
  toJSON = JS.String . TL.toStrict . TLB.toLazyText . JSE.encodeToTextBuilder . unTextJSON
instance JS.FromJSON TextJSON where
  parseJSON = JS.withText "TextJSON" $ either fail (return . TextJSON) . JS.eitherDecodeStrict . TE.encodeUtf8

newtype ClientStore = ClientStore [(String, [Card])]

instance JS.FromJSON ClientStore where
  parseJSON = JS.withArray "client_store" $
    fmap ClientStore . mapM (JS.withObject "client_store" $ \cs -> do
      s <- cs JS..: "client_store"
      n <- s JS..: "name"
      TextJSON d <- s JS..: "data"
      c <- JS.withObject "data" (JS..: "cards") d
      return (n, c)) . V.toList

data Card = Card
  { cardUrl :: Maybe BS.ByteString
  , cardData :: [(T.Text, T.Text)]
  , cardTitle :: T.Text
  }

instance JS.FromJSON Card where
  parseJSON = JS.withObject "card" $ \c -> do
    l <- c JS..: "listing"
    u <- l JS..:? "url"
    d <- l JS..:? "data" JS..!= []
      >>= mapM (\d ->
        (,) <$> d JS..: "name"
            <*> (val =<< d JS..: "value"))
    t <- c JS..: "title"
    return $ Card
      { cardUrl = TE.encodeUtf8 <$> u
      , cardData = filter ok d
      , cardTitle = t
      }
    where
    val (JS.String t) = return t
    val (JS.Number n) = return $ T.pack $ show n
    val v = JS.typeMismatch "card value" v
    ok (k, v) = T.all ('-' /=) k && not (T.null v)

data List = List
  { listPageNext :: Bool
  , listModels :: [Model]
  }

instance JS.FromJSON List where
  parseJSON = JS.withObject "list" $ \l ->
    List
      <$> (l JS..: "master" >>= (JS..: "pagination") >>= (JS..: "next"))
      <*> (l JS..: "models")

data Model = Model
  { modelClass :: T.Text
  , modelId :: Int
  , modelOptions :: OptionMap JS.Value
  }

instance JS.FromJSON Model where
  parseJSON = JS.withObject "model" $ \o -> do
    m <- o JS..: "model"
    c <- m JS..: "class_name"
    i <- m JS..: "id"
    l <- o JS..: "options"
    return $ Model
      { modelClass = c
      , modelId = i
      , modelOptions = l
      }

newtype OptionMap a = OptionMap (HM.HashMap UUID.UUID a)

instance JS.FromJSON a => JS.FromJSON (OptionMap a) where
  parseJSON = JS.withObject "options" $ fmap (OptionMap . HM.fromList) . mapM (runKleisli $ (Kleisli uuid *** Kleisli JS.parseJSON)) . HM.toList
    where uuid t = maybe (JS.typeMismatch "uuid" $ JS.String t) return $ UUID.fromText t

getModelOption :: (Monad m, JS.FromJSON a) => UUID.UUID -> Model -> m a
getModelOption u Model{ modelId = i, modelOptions = OptionMap o } =
  maybe (fail $ "Couldn't find option " ++ show u ++ " for model " ++ show i) return
    $ JS.parseMaybe JS.parseJSON =<< HM.lookup u o

newtype Attachments = Attachments [Attachment]

instance JS.FromJSON Attachments where
  parseJSON = JS.withObject "attachments" $ fmap Attachments . (JS..: "docs")
    
data Attachment = Attachment
  { _attachmentLink :: String
  }

instance JS.FromJSON Attachment where
  parseJSON = JS.withObject "doc" $ fmap Attachment . (JS..: "document_link")


tagText :: Monoid a => [HTS.TagTree a] -> a
tagText t = mconcat [ s | HTS.TagText s <- HTS.flattenTree t ]

tagHText :: [HTS.TagTree String] -> [String]
tagHText t = [ tagText tt | HTS.TagBranch ['h',d] _ tt <- t, d >= '1' && d <= '4' ]

tagAttachments :: [HTS.TagTree String] -> [(HC.Request, String)]
tagAttachments t =
  [ (url, name)
  | HTS.TagBranch "a" (("href", urls) : _) [HTS.TagLeaf (HTS.TagText name)] <- t
  , url <- HC.parseUrl urls
  ]

runGrantRequest :: String -> String -> Model -> RunM ()
runGrantRequest ncpf name m = do
  req <- asks stateRequest
  cache <- asks stateCache
  now <- asks stateTime
  let preq = req
        { HC.path = "/grant_requests/" <> BSC.pack (show $ modelId m)
        , HC.queryString = "?printable=1"
        }
      (cookie, _) = HC.computeCookieString preq (fold $ HC.cookieJar preq) now True
  runIn "printable" (FP.addExtension (ncpf ++ '_' : name) ".pdf") $ do
    out <- asks stateOutput
    liftIO $ callProcess "wkhtmltopdf" $
      [ "-q"
      , "-s", "Letter", "-B", "10cm", "-T", "10cm"
      , "--cache-dir", cache
      , "--custom-header", "Cookie", BSC.unpack cookie, "--no-custom-header-propagation"
      , URI.uriToString id (HC.getUri preq) ""
      , out
      ]
  m' <- httpRequestJSON req
    { HC.path = "/stencils/grant_request/show/" <> BSC.pack (show $ modelId m)
    }
  Attachments atts <- getModelOption attachmentUUID m'
  forM_ atts $ \(Attachment link) -> do
    (areq, aname) <- case
        [ (ureq, fname)
        | (url, fname) <- tagAttachments $ HTS.parseTree link
        , let path = HC.path url
        , BSC.isPrefixOf "/s3/model_doc/" path
        , let ureq = req{ HC.path = path }
        , HC.getUri url == HC.getUri ureq
        ] of
      [a] -> return a
      _ -> fail $ "Could not process attachment of model " ++ show (modelId m) ++ ": " ++ link
    runIn "attachment" (ncpf ++ '_' : aname) $ httpRequestDownload areq

runModel :: Model -> RunM ()
runModel m@Model{ modelClass = "GrantRequest" } = do
  title <- getModelOption titleUUID m
  (name, ncpf) <- case tagHText $ HTS.parseTree title of
    name : ncpf : _ -> return (name, takeWhile (\c -> isAlphaNum c || c == '-' || c == '_') ncpf)
    _ -> fail $ "Could not process title of model " ++ show (modelId m) ++ ": " ++ title
  runIn "grant request" (ncpf ++ '_' : name) $
    runGrantRequest ncpf name m
runModel Model{ modelClass = c } = fail $ "unknown model class: " ++ T.unpack c

runCard :: Card -> Int -> RunM ()
runCard card@Card{ cardUrl = Just url@"/grant_requests" } page = do
  liftIO $ putStrLn $ "\tpage " ++ show page
  req <- asks stateRequest
  l <- httpRequestJSON req
    { HC.path = url <> ".json"
    , HC.queryString = H.renderQuery True $ H.toQuery $ [("page", T.pack $ show page), ("stencil", "true")] ++ cardData card
    }
  forM_ (listModels l) $ runModel
  when (listPageNext l) $ runCard card (succ page)
runCard _ _ = return ()

runDashboard :: String -> RunM ()
runDashboard name = do
  req <- asks stateRequest

  ClientStore cs <- httpRequestJSON req
    { HC.path = "/client_stores.json/"
    , HC.queryString = "?client_store_type=dashboard"
    }
  cards <- maybe (fail $ "Dashboard " ++ name ++ " not found in " ++ show (map fst cs)) return $
    lookup name cs
  
  liftIO $ hPutStrLn stderr "Processing cards..."
  forM_ cards $ \card -> do
    let title = T.unpack $ tagText $ HTS.parseTree $ cardTitle card
    runIn "card" title $ runCard card 1

data Opts = Opts
  { optDomain :: String
  , optUser :: Maybe String
  , optPass :: Maybe String
  , optOutput :: FilePath
  }

defOpts :: Opts
defOpts = Opts
  { optDomain = "artplaceamerica"
  , optUser = Nothing
  , optPass = Nothing
  , optOutput = ""
  }

options :: [Opt.OptDescr (Opts -> Opts)]
options =
  [ Opt.Option "d" ["domain"]
      (Opt.ReqArg (\i o -> o{ optDomain = i }) "NAME")
      ("subdomain under fluxx.io [" ++ optDomain defOpts ++ "]")
  , Opt.Option "u" ["user"]
      (Opt.ReqArg (\i o -> o{ optUser = Just i }) "NAME")
      "login with username"
  , Opt.Option "p" ["pass"]
      (Opt.ReqArg (\i o -> o{ optPass = Just i }) "PASS")
      "login with password"
  , Opt.Option "o" ["output"]
      (Opt.ReqArg (\i o -> o{ optOutput = i }) "DIRECTORY")
      "output to directory (must exist) [current]"
  ]

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  (opts, dashname) <- case Opt.getOpt Opt.Permute options args of
    (o, [a], []) ->
      return (foldl' (flip ($)) defOpts o, a)
    (_, _, errs) -> do
      mapM_ (hPutStrLn stderr) errs
      hPutStrLn stderr $ Opt.usageInfo ("Usage: "  ++ prog ++ " DASHBOARD\n") options
      exitFailure
  baseReq <- HC.parseUrl ("https://" ++ optDomain opts ++ ".fluxx.io/")
  hc <- HC.newManager HCT.tlsManagerSettings
  now <- getCurrentTime

  hd <- getAppUserDataDirectory "fluxx.hs"
  createDirectoryIfMissing False hd
  let cjf = hd FP.</> "cookies"
      cache = hd FP.</> "cache"
  createDirectoryIfMissing False cache

  cj <- maybe
    ((fmap (`HC.evictExpiredCookies` now) . readIO =<< readFile cjf)
      `onException` hPutStrLn stderr "You may need to use -U to login")
    (\u -> do
      p <- maybe (IL.runInputT IL.defaultSettings $ fromMaybe "" <$> IL.getPassword (Just '*') "password: ") return $ optPass opts
      when (null p) $ exitFailure
      HC.responseCookieJar <$> HC.httpNoBody baseReq
        { HC.path = "/user_sessions"
        , HC.method = H.methodPost
        , HC.requestHeaders = (H.hContentType, "application/x-www-form-urlencoded") : HC.requestHeaders baseReq
        , HC.requestBody = HC.RequestBodyBS $ H.renderQuery False $ H.toQuery
          [ ("user_session[login]" :: BS.ByteString, u)
          , ("user_session[password]", p)
          ]
        } hc)
    (optUser opts)
  unless (any ((==) "user_credentials" . HC.cookie_name) $ HC.destroyCookieJar cj) $ fail "Login failed"
  writeFile cjf $ show cj

  r <- runReaderT (runExceptT $ runM $ runDashboard dashname) State
    { stateTime = now
    , stateCache = cache
    , stateHTTP = hc
    , stateRequest = baseReq{ HC.cookieJar = Just cj }
    , stateOutput = optOutput opts
    }
  either (\e -> do
    hPutStrLn stderr e
    exitFailure)
    return r
