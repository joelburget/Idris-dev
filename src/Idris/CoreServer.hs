{-# LANGUAGE OverloadedStrings, DeriveGeneric, ConstraintKinds #-}
-- module Idris.CoreServer (startCoreServer) where
module Idris.CoreServer where

import Cheapskate.Html
import Cheapskate.Types as C (Doc)
import Conduit as C
import Control.Concurrent (forkOS)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.Trans.State.Strict
import Data.Aeson (encode, eitherDecode, ToJSON(toJSON), FromJSON, Value(..))
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.HashMap.Strict as H
import Data.IORef
import Data.List (sort)
import GHC.Generics (Generic)
import Network.HTTP.Types
import Network.Wai as W
import Network.Wai.Handler.Warp as W
import Text.Blaze.Renderer.Text (renderMarkup)

import Idris.Core.Evaluate
import Idris.Core.Execute (execute)
import Idris.Core.TT
import Idris.Core.Constraints

import Idris.AbsSyntaxTree (IState(..), PTerm)
import Idris.Apropos (apropos', Docs)
import Idris.Delaborate (delabTy, isUN)
import Idris.Docstrings (Docstring, overview)
import Idris.TypeSearch (simpleSearch)

import Util.Pretty (Doc)

corePort :: Int
corePort = 4296

type Mon m = (MonadIO m, MonadResource m)

data ServerState = ServerState
    { _response :: Maybe BL.ByteString
    , _status   :: Maybe Status
    }

response :: Lens' ServerState (Maybe BL.ByteString)
response = lens _response (\ss v -> ss { _response = v })

type Server = StateT (IState, ServerState) (ErrorT Err IO)

show' :: Show a => a -> BL.ByteString
show' = B8.pack . show

app :: IORef IState -> Request -> IO Response
app ref req = runResourceT $ getBody req $= parse $$ respond ref

getBody :: Mon m => Request -> Source m BL.ByteString
getBody req = do
    Just body <- liftIO $ requestBody req $$ await
    C.yield $ BL.fromStrict body

parse :: Mon m => Conduit BL.ByteString m (Either String CoreCommand)
parse = do
    Just body <- await
    C.yield $ eitherDecode body

initServerState = ServerState Nothing Nothing

respond :: Mon m
        => IORef IState
        -> Sink (Either String CoreCommand) m Response
respond ref = do
    Just cmd <- await
    st <- liftIO $ readIORef ref
    case cmd of
        Left err ->
            return $ responseLBS badRequest400 headers $ show' err
        Right c -> do
            res <- liftIO $
                runErrorT $ execStateT (processCmd c) (st, initServerState)
            case res of
                Left err ->
                    return $ responseLBS badRequest400 headers $ show' err
                Right (st', serverSt) -> do
                    liftIO $ writeIORef ref st'
                    case serverSt^.response of
                        Nothing -> return $ responseLBS ok200 headers ""
                        Just x -> return $ responseLBS ok200 headers x

headers =
    [ (hContentType, "text/json")
    , ("Access-Control-Allow-Origin", "*")
    ]

startCoreServer :: IState -> [FilePath] -> IO ()
startCoreServer ist files = do
    ref <- newIORef ist
    void $ forkOS $ run corePort $ app ref

setResponse :: ToJSON a => a -> Server ()
setResponse r = _2.response .= Just (encode r)

defs :: IState -> [(Name, Def)]
defs = ctxtAlist . tt_ctxt

docs :: IState -> [(Name, Docs)]
docs = toAlist . idris_docstrings

instance ToJSON C.Doc where
    toJSON = String . toStrict . renderMarkup . renderDoc

processCmd :: CoreCommand -> Server ()
processCmd (Define name binder term) = undefined

processCmd (SearchName text) = do
    (ist, _) <- get
    let names = apropos' text (defs ist) (docs ist)
        aproposInfo :: [(Name, PTerm, Maybe Docstring)]
        aproposInfo = [ (n,
                         delabTy ist n,
                         fmap (overview . fst) (lookupCtxtExact n (idris_docstrings ist)))
                      | n <- sort names, isUN n ]
    setResponse $ H.fromList [("results" :: T.Text, aproposInfo)]

processCmd (SearchType term) = do
    (ist, _) <- get
    Right (res, ist') <- liftIO $
        runErrorT $ runStateT (simpleSearch term) ist
    _1 .= ist'
    setResponse res

processCmd (Eval term) = undefined

data CoreCommand
    -- | Load a module.
    --
    -- I guess we have explicit import?
    --
    -- Is Name the right type?
    -- = Load Name

    -- | Make a new term!
    --
    -- Turn this into an RBind
    = Define Name (Binder Raw) Raw
    -- | Find this (partial) name
    --
    -- Should this be Text? Should other Names be Text?
    | SearchName T.Text
    -- | Find a term that matches this type!
    --
    -- Use Vars for holes?
    | SearchType PTerm
    | Eval Raw
    deriving Generic

instance ToJSON CoreCommand
instance FromJSON CoreCommand
