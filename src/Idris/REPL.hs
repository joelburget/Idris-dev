{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveFunctor,
             PatternGuards, CPP, ConstraintKinds #-}

module Idris.REPL where

import Idris.AbsSyntax
import Idris.ASTUtils
import Idris.Apropos (apropos)
import Idris.REPLParser
import Idris.ElabDecls
import Idris.ElabTerm
import Idris.Erasure
import Idris.Error
import Idris.ErrReverse
import Idris.Delaborate
import Idris.Docstrings (Docstring, overview, renderDocstring)
import Idris.IdrisDoc
import Idris.Prover
import Idris.Parser hiding (indent)
import Idris.Primitives
import Idris.Coverage
import Idris.Docs hiding (Doc)
import Idris.Help
import Idris.Completion
import qualified Idris.IdeSlave as IdeSlave
import qualified Idris.Server as Server
import Idris.CoreServer (startCoreServer)
import Idris.Chaser
import Idris.Imports
import Idris.Colours hiding (colourise)
import Idris.Inliner
import Idris.CaseSplit
import Idris.DeepSeq
import Idris.Output
import Idris.Interactive
import Idris.WhoCalls
import Idris.TypeSearch (searchByType, searchPred, defaultScoreFunction)
import Idris.Util

import Paths_idris
import Version_idris (gitHash)
import Util.System
import Util.DynamicLinker
import Util.Net (listenOnLocalhost)
import Util.Pretty hiding ((</>))

import Idris.Core.Evaluate
import Idris.Core.Execute (execute)
import Idris.Core.TT
import Idris.Core.Constraints

import IRTS.Compiler
import IRTS.CodegenCommon

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.List.Split (splitOn)
import qualified Data.Text as T

import Text.Trifecta.Result(Result(..))

import Conduit as C
import Data.IORef
import Network.HTTP.Types
import Network.Wai as W
import Network.Wai.Handler.Warp as W

-- import RTS.SC
-- import RTS.Bytecode
-- import RTS.PreC
-- import RTS.CodegenC
#ifdef IDRIS_LLVM
import LLVM.General.Target
#else
import Util.LLVMStubs
#endif
import Control.Arrow (right)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans.Error (ErrorT(..))
import Control.Monad.Trans.State.Strict ( StateT, execStateT, evalStateT, get, put )
import Control.Monad.Trans ( lift )
import Data.Aeson (encode, eitherDecode, toJSON)
import Data.Maybe
import Data.List
import Data.Char
import Data.Version
import Data.Word (Word)
import Data.Either (partitionEithers)
import Network
import System.Console.Haskeline as H
import System.FilePath
import System.Exit
import System.Environment
import System.Process
import System.Directory
import System.IO as IO

import Debug.Trace

clientPort :: PortID
clientPort = PortNumber 4294

jsonPort :: Int
jsonPort = 4295

-- | Run the REPL
repl :: IState -- ^ The initial state
     -> [FilePath] -- ^ The loaded modules
     -> InputT Idris ()
repl orig mods
   = -- H.catch
     do let quiet = opt_quiet (idris_options orig)
        i <- lift getIState
        let colour = idris_colourRepl i
        let theme = idris_colourTheme i
        let mvs = idris_metavars i
        let prompt = if quiet
                        then ""
                        else showMVs colour theme mvs ++
                             let str = mkPrompt mods ++ ">" in
                             (if colour then colourisePrompt theme str else str) ++ " "
        x <- H.catch (getInputLine prompt)
                     (ctrlC (return Nothing))
        case x of
            Nothing -> do lift $ when (not quiet) (iputStrLn "Bye bye")
                          return ()
            Just input -> -- H.catch
                do ms <- H.catch (lift $ processInput input orig mods)
                                 (ctrlC (return (Just mods)))
                   case ms of
                        Just mods -> repl orig mods
                        Nothing -> return ()
--                             ctrlC)
--       ctrlC
   where ctrlC :: InputT Idris a -> SomeException -> InputT Idris a
         ctrlC act e = do lift $ iputStrLn (show e)
                          act -- repl orig mods

         showMVs c thm [] = ""
         showMVs c thm ms = "Metavariables: " ++
                                 show' 4 c thm (map fst ms) ++ "\n"

         show' 0 c thm ms = let l = length ms in
                          "... ( + " ++ show l
                             ++ " other"
                             ++ if l == 1 then ")" else "s)"
         show' n c thm [m] = showM c thm m
         show' n c thm (m : ms) = showM c thm m ++ ", " ++
                                  show' (n - 1) c thm ms

         showM c thm n = if c then colouriseFun thm (show n)
                              else show n

type ConnectionHandler = IState
                      -> IState
                      -> IO.Handle
                      -> FilePath
                      -> String
                      -> IO (IState, FilePath)

type Mon m = (MonadIO m, MonadResource m)

startJsonServer :: IState -> [FilePath] -> Idris ()
startJsonServer orig files = runIO $ do
    ref <- newIORef (orig, head files)
    void $ forkOS $ serverLoop ref
  where serverLoop :: IORef (IState, FilePath) -> IO ()
        serverLoop ref = run jsonPort $ app ref

        app :: IORef (IState, FilePath) -> Request -> IO Response
        app ref req = runResourceT $ getBody req $= parse $$ respond ref

        getBody :: Mon m => Request -> Source m BL.ByteString
        getBody req = do
            Just body <- liftIO $ requestBody req $$ await
            C.yield $ BL.fromStrict body

        show' :: Show a => a -> BL.ByteString
        show' = B8.pack . show

        parse :: Mon m => Conduit BL.ByteString m (Either String Command)
        parse = do
            Just body <- await
            let decoded = eitherDecode body
            C.yield decoded
            -- C.yield $ eitherDecode body

        respond :: Mon m
                => IORef (IState, FilePath)
                -> Sink (Either String Command) m Response
        respond ref = do
            Just cmd <- await
            liftIO $ do
                putStrLn "cmd:"
                print $ toJSON cmd
                putStrLn ""
            (ist, file) <- liftIO $ readIORef ref
            liftIO $ putStrLn "got ist and file"
            case cmd of
                Left err -> do
                    liftIO $ putStrLn "first"
                    return $ responseLBS badRequest400 headers $ show' err
                Right c -> do
                    res <- liftIO $
                        runErrorT $ evalStateT (processJson file c) ist
                    case res of
                        Left err -> do
                            liftIO $ putStrLn "second"
                            return $ responseLBS badRequest400 headers $ show' err
                        Right (ist', file', output) -> do
                            liftIO $ putStrLn "third"
                            liftIO $ writeIORef ref (ist', file')
                            return $ responseLBS ok200 headers $ encode output

        headers = [ (hContentType, BL.toStrict $ B8.pack "text/json") ]

        processJson :: FilePath
                    -> Command
                    -> Idris (IState, FilePath, String)
        processJson file cmd = do
            -- TODO(joel) this is nasty.
            (fp, h) <- runIO $ IO.openTempFile "." "processJson"
            process h file cmd
            runIO $ hClose h
            bs <- runIO $ IO.readFile fp
            ist <- getIState
            return (ist, file, bs)

startServer :: PortID -> ConnectionHandler -> IState -> [FilePath] -> Idris ()
startServer port handler orig files = void $ runIO $ forkOS serverLoop
  where serverLoop :: IO ()
        serverLoop = withSocketsDo $
            listenOnLocalhost port >>= loop fn orig

        -- TODO(joel) why do we take in these files then only possibly use
        -- one?
        fn = safeHead "" files

        loop fn ist sock = do
            (h, _, _) <- accept sock
            cmd <- hGetLine h
            (ist', fn') <- handler orig ist h fn cmd
            hClose h
            loop fn' ist' sock

-- | Run the REPL server
startClientServer :: IState -> [FilePath] -> Idris ()
startClientServer = startServer clientPort processNetCmd

processJsonCmd :: IState -- ^ original state?
               -> IState -- ^ current state?
               -> IO.Handle -- ^ output (socket) handle
               -> FilePath -- ^ main file?
               -> String -- ^ command to execute
               -> IO (IState, FilePath) -- ^ (new state, new file)?
processJsonCmd orig i h file cmd =
    case eitherDecode $ {- BL.fromStrict $ -} B8.pack cmd of
        Left err -> hPrint h err >> return (i, file)
        Right c -> do
            res <- runErrorT $ evalStateT (processJson file c) i
            case res of
                Left err -> hPrint h err >> return (i, file)
                Right x -> return x
  where processJson :: FilePath -> Command -> Idris (IState, FilePath)
        processJson file cmd = do
            process h file cmd
            ist <- getIState
            return (ist, file)

processNetCmd :: IState -> IState -> IO.Handle -> FilePath -> String ->
                 IO (IState, FilePath)
processNetCmd orig i h fn cmd
    = do res <- case parseCmd i "(net)" cmd of
                  Failure err -> return (Left (Msg " invalid command"))
                  Success c -> runErrorT $ evalStateT (processNet fn c) i
         case res of
              Right x -> return x
              Left err -> do hPrint h err
                             return (i, fn)
  where
    processNet :: FilePath -> Command -> Idris (IState, FilePath)
    processNet fn Reload = processNet fn (Load fn Nothing)
    processNet fn (Load f toline) =
        do let ist = orig { idris_options = idris_options i
                          , idris_colourTheme = idris_colourTheme i
                          , idris_colourRepl = False
                          }
           putIState ist
           setErrContext True
           setOutH h
           setQuiet True
           setVerbose False
           mods <- loadInputs h [f] toline
           ist <- getIState
           return (ist, f)
    processNet fn c = do process h fn c
                         ist <- getIState
                         return (ist, fn)

-- | Run a command on the server on localhost
runClient :: String -> IO ()
runClient str = withSocketsDo $ do
                  h <- connectTo "localhost" clientPort
                  hPutStrLn h str
                  resp <- hGetResp "" h
                  putStr resp
                  hClose h
    where hGetResp acc h = do eof <- hIsEOF h
                              if eof then return acc
                                     else do l <- hGetLine h
                                             hGetResp (acc ++ l ++ "\n") h

-- | Run the IdeSlave
startDependent :: IState -> [FilePath] -> Idris ()
startDependent orig mods
  = do i <- getIState
       when (mods /= []) (isetPrompt (mkPrompt mods))
       case idris_outputmode i of
         IdeSlave _ -> ideslave orig mods
         Server   _ -> server orig mods

server :: IState -> [FilePath] -> Idris ()
server orig mods = do
    idrisCatch (do
        l <- runIO getLine
        (sexp, id) <- case Server.parseMessage l of
            Left err -> ierror err
            Right (sexp, id) -> return (sexp, id)
        modIState $ \i -> i { idris_outputmode = Server id }
        idrisCatch (do
            let fn = case mods of
                    (f:_) -> f
                    _ -> ""
            case IdeSlave.sexpToCommand sexp of
                -- Just cmd -> runServerCommand orig fn mods cmd
                Just cmd -> runIdeSlaveCommand undefined orig fn mods cmd
                -- TODO(joel) - better error message
                Nothing  -> iPrintError "did not understand")
            (iPrintError . show))
        (iPrintError . show)
    server orig mods

-- | Loop waiting for commands to execute
ideslave :: IState -> [FilePath] -> Idris ()
ideslave orig mods
  = do idrisCatch
         (do l <- runIO getLine
             (sexp, id) <- case IdeSlave.parseMessage l of
                             Left err -> ierror err
                             Right (sexp, id) -> return (sexp, id)
             modIState $ \i -> i { idris_outputmode = (IdeSlave id) }
             idrisCatch -- to report correct id back!
               (do let fn = case mods of
                              (f:_) -> f
                              _ -> ""
                   case IdeSlave.sexpToCommand sexp of
                     Just cmd -> runIdeSlaveCommand id orig fn mods cmd
                     Nothing  -> iPrintError "did not understand" )
               (iPrintError . show))
         (iPrintError . show)
       ideslave orig mods

runServerCommand :: IState
                 -> FilePath
                 -> [FilePath]
                 -> Server.ServerCommand
                 -> Idris ()
runServerCommand orig fn mods (IdeSlave.Interpret cmd) = do
    c <- colourise
    i <- getIState
    case parseCmd undefined "(server)" cmd of
        Failure err -> iPrintError $ show (fixColour c err)
        Success (Prove n') -> do
            iPrintResult ""
            idrisCatch
              (process stdout fn (Prove n'))
              (\e -> getIState >>= ihRenderError stdout . flip pprintErr e)
            isetPrompt (mkPrompt mods)
        Success cmd -> idrisCatch
            (ideslaveProcess fn cmd)
            (\e -> getIState >>= ihRenderError stdout . flip pprintErr e)

-- | Run IDESlave commands
runIdeSlaveCommand :: Integer -- ^^ The continuation ID for the client
                   -> IState -- ^^ The original IState
                   -> FilePath -- ^^ The current open file
                   -> [FilePath] -- ^^ The currently loaded modules
                   -> IdeSlave.IdeSlaveCommand -- ^^ The command to process
                   -> Idris ()
runIdeSlaveCommand id orig fn mods (IdeSlave.Interpret cmd) =
  do c <- colourise
     i <- getIState
     case parseCmd i "(input)" cmd of
       Failure err -> iPrintError $ show (fixColour c err)
       Success (Prove n') -> do
           iPrintResult ""
           idrisCatch
             (process stdout fn (Prove n'))
             (\e -> getIState >>= ihRenderError stdout . flip pprintErr e)
           isetPrompt (mkPrompt mods)
       Success cmd -> idrisCatch
                        (ideslaveProcess fn cmd)
                        (\e -> getIState >>= ihRenderError stdout . flip pprintErr e)
runIdeSlaveCommand id orig fn mods (IdeSlave.REPLCompletions str) =
  do (unused, compls) <- replCompletion (reverse str, "")
     let good = IdeSlave.SexpList [IdeSlave.SymbolAtom "ok",
                                   IdeSlave.toSExp (map replacement compls,
                                   reverse unused)]
     runIO $ putStrLn $ IdeSlave.convSExp "return" good id
runIdeSlaveCommand id orig fn mods (IdeSlave.LoadFile filename toline) =
  do i <- getIState
     clearErr
     putIState (orig { idris_options = idris_options i,
                       idris_outputmode = (IdeSlave id) })
     loadInputs stdout [filename] toline
     isetPrompt (mkPrompt [filename])
     -- Report either success or failure
     i <- getIState
     case (errSpan i) of
       Nothing -> let msg = maybe (IdeSlave.SexpList [IdeSlave.SymbolAtom "ok",
                                                      IdeSlave.SexpList []])
                                  (\fc -> IdeSlave.SexpList [IdeSlave.SymbolAtom "ok",
                                                             IdeSlave.toSExp fc])
                                  (idris_parsedSpan i)
                  in runIO . putStrLn $ IdeSlave.convSExp "return" msg id
       Just x -> iPrintError $ "didn't load " ++ filename
     ideslave orig [filename]
runIdeSlaveCommand id orig fn mods (IdeSlave.TypeOf name) =
  case splitName name of
    Left err -> iPrintError err
    Right n -> process stdout "(ideslave)"
                 (Check (PRef (FC "(ideslave)" (0,0) (0,0)) n))
  where splitName :: String -> Either String Name
        splitName s = case reverse $ splitOn "." s of
                        [] -> Left ("Didn't understand name '" ++ s ++ "'")
                        [n] -> Right $ sUN n
                        (n:ns) -> Right $ sNS (sUN n) ns
runIdeSlaveCommand id orig fn mods (IdeSlave.DocsFor name) =
  case parseConst orig name of
    Success c -> process stdout "(ideslave)" (DocStr (Right c))
    Failure _ ->
     case splitName name of
       Left err -> iPrintError err
       Right n -> process stdout "(ideslave)" (DocStr (Left n))
runIdeSlaveCommand id orig fn mods (IdeSlave.CaseSplit line name) =
  process stdout fn (CaseSplitAt False line (sUN name))
runIdeSlaveCommand id orig fn mods (IdeSlave.AddClause line name) =
  process stdout fn (AddClauseFrom False line (sUN name))
runIdeSlaveCommand id orig fn mods (IdeSlave.AddProofClause line name) =
  process stdout fn (AddProofClauseFrom False line (sUN name))
runIdeSlaveCommand id orig fn mods (IdeSlave.AddMissing line name) =
  process stdout fn (AddMissing False line (sUN name))
runIdeSlaveCommand id orig fn mods (IdeSlave.MakeWithBlock line name) =
  process stdout fn (MakeWith False line (sUN name))
runIdeSlaveCommand id orig fn mods (IdeSlave.ProofSearch r line name hints depth) =
  doProofSearch stdout fn False r line (sUN name) (map sUN hints) depth
runIdeSlaveCommand id orig fn mods (IdeSlave.MakeLemma line name) =
  case splitName name of
    Left err -> iPrintError err
    Right n -> process stdout fn (MakeLemma False line n)
runIdeSlaveCommand id orig fn mods (IdeSlave.Apropos a) =
  process stdout fn (Apropos a)
runIdeSlaveCommand id orig fn mods (IdeSlave.GetOpts) =
  do ist <- getIState
     let opts = idris_options ist
     let impshow = opt_showimp opts
     let errCtxt = opt_errContext opts
     let options = (IdeSlave.SymbolAtom "ok",
                    [(IdeSlave.SymbolAtom "show-implicits", impshow),
                     (IdeSlave.SymbolAtom "error-context", errCtxt)])
     runIO . putStrLn $ IdeSlave.convSExp "return" options id
runIdeSlaveCommand id orig fn mods (IdeSlave.SetOpt IdeSlave.ShowImpl b) =
  do setImpShow b
     let msg = (IdeSlave.SymbolAtom "ok", b)
     runIO . putStrLn $ IdeSlave.convSExp "return" msg id
runIdeSlaveCommand id orig fn mods (IdeSlave.SetOpt IdeSlave.ErrContext b) =
  do setErrContext b
     let msg = (IdeSlave.SymbolAtom "ok", b)
     runIO . putStrLn $ IdeSlave.convSExp "return" msg id
runIdeSlaveCommand id orig fn mods (IdeSlave.Metavariables cols) =
  do ist <- getIState
     let mvs = reverse $ map fst (idris_metavars ist) \\ primDefs
     let ppo = ppOptionIst ist
     let mvarTys = map (delabTy ist) mvs
     let res = (IdeSlave.SymbolAtom "ok",
                zipWith (\ n (prems, concl) -> (n, prems, concl))
                        (map (IdeSlave.StringAtom . show) mvs)
                        (map (sexpGoal ist cols ppo [] . getGoal) mvarTys))
     runIO . putStrLn $ IdeSlave.convSExp "return" res id
  where getGoal :: PTerm -> ([(Name, PTerm)], PTerm)
        getGoal (PPi _ n t sc) = let (prems, conc) = getGoal sc
                                 in ((n, t):prems, conc)
        getGoal tm = ([], tm)
        sexpGoal :: IState -> Int -> PPOption -> [Name] -> ([(Name, PTerm)], PTerm)
                 -> ([(String, String, SpanList OutputAnnotation)],
                     (String, SpanList OutputAnnotation))
        sexpGoal ist cols ppo ns ([],        concl) =
          let infixes = idris_infixes ist
              concl' = displaySpans . renderPretty 0.9 cols . fmap (fancifyAnnots ist) $
                       pprintPTerm ppo (zip ns (repeat False)) [] infixes concl
          in ([], concl')
        sexpGoal ist cols ppo ns ((n, t):ps, concl) =
          let n'          = case n of
                              NS (UN nm) ns -> str nm
                              UN nm | ('_':'_':_) <- str nm -> "_"
                                    | otherwise -> str nm
                              _ -> "_"
              (t', spans) = displaySpans . renderPretty 0.9 cols . fmap (fancifyAnnots ist) $
                            pprintPTerm ppo (zip ns (repeat False)) [] (idris_infixes ist) t
              rest        = sexpGoal ist cols ppo (n:ns) (ps, concl)
          in ((n', t', spans) : fst rest, snd rest)
runIdeSlaveCommand id orig fn mods (IdeSlave.WhoCalls n) =
  case splitName n of
       Left err -> iPrintError err
       Right n -> do calls <- whoCalls n
                     ist <- getIState
                     let msg = (IdeSlave.SymbolAtom "ok",
                                map (\ (n,ns) -> (pn ist n, map (pn ist) ns)) calls)
                     runIO . putStrLn $ IdeSlave.convSExp "return" msg id
  where pn ist = displaySpans .
                 renderPretty 0.9 1000 .
                 fmap (fancifyAnnots ist) .
                 prettyName True []
runIdeSlaveCommand id orig fn mods (IdeSlave.CallsWho n) =
  case splitName n of
       Left err -> iPrintError err
       Right n -> do calls <- callsWho n
                     ist <- getIState
                     let msg = (IdeSlave.SymbolAtom "ok",
                                map (\ (n,ns) -> (pn ist n, map (pn ist) ns)) calls)
                     runIO . putStrLn $ IdeSlave.convSExp "return" msg id
  where pn ist = displaySpans .
                 renderPretty 0.9 1000 .
                 fmap (fancifyAnnots ist) .
                 prettyName True []

splitName :: String -> Either String Name
splitName s = case reverse $ splitOn "." s of
                [] -> Left ("Didn't understand name '" ++ s ++ "'")
                [n] -> Right $ sUN n
                (n:ns) -> Right $ sNS (sUN n) ns

ideslaveProcess :: FilePath -> Command -> Idris ()
ideslaveProcess fn Help = process stdout fn Help
ideslaveProcess fn (ChangeDirectory f) = do process stdout fn (ChangeDirectory f)
                                            iPrintResult "changed directory to"
ideslaveProcess fn (Eval t) = process stdout fn (Eval t)
ideslaveProcess fn (ExecVal t) = process stdout fn (ExecVal t)
ideslaveProcess fn (Check (PRef x n)) = process stdout fn (Check (PRef x n))
ideslaveProcess fn (Check t) = process stdout fn (Check t)
ideslaveProcess fn (DocStr n) = process stdout fn (DocStr n)
ideslaveProcess fn Universes = process stdout fn Universes
ideslaveProcess fn (Defn n) = do process stdout fn (Defn n)
                                 iPrintResult ""
ideslaveProcess fn (TotCheck n) = process stdout fn (TotCheck n)
ideslaveProcess fn (DebugInfo n) = do process stdout fn (DebugInfo n)
                                      iPrintResult ""
ideslaveProcess fn (Search t) = process stdout fn (Search t)
ideslaveProcess fn (Spec t) = process stdout fn (Spec t)
-- RmProof and AddProof not supported!
ideslaveProcess fn (ShowProof n') = process stdout fn (ShowProof n')
ideslaveProcess fn (HNF t) = process stdout fn (HNF t)
--ideslaveProcess fn TTShell = process stdout fn TTShell -- need some prove mode!
ideslaveProcess fn (TestInline t) = process stdout fn (TestInline t)

ideslaveProcess fn Execute = do process stdout fn Execute
                                iPrintResult ""
ideslaveProcess fn (Compile codegen f) = do process stdout fn (Compile codegen f)
                                            iPrintResult ""
ideslaveProcess fn (LogLvl i) = do process stdout fn (LogLvl i)
                                   iPrintResult ""
ideslaveProcess fn (Pattelab t) = process stdout fn (Pattelab t)
ideslaveProcess fn (Missing n) = process stdout fn (Missing n)
ideslaveProcess fn (DynamicLink l) = do process stdout fn (DynamicLink l)
                                        iPrintResult ""
ideslaveProcess fn ListDynamic = do process stdout fn ListDynamic
                                    iPrintResult ""
ideslaveProcess fn Metavars = process stdout fn Metavars
ideslaveProcess fn (SetOpt ErrContext) = do process stdout fn (SetOpt ErrContext)
                                            iPrintResult ""
ideslaveProcess fn (UnsetOpt ErrContext) = do process stdout fn (UnsetOpt ErrContext)
                                              iPrintResult ""
ideslaveProcess fn (SetOpt ShowImpl) = do process stdout fn (SetOpt ShowImpl)
                                          iPrintResult ""
ideslaveProcess fn (UnsetOpt ShowImpl) = do process stdout fn (UnsetOpt ShowImpl)
                                            iPrintResult ""
ideslaveProcess fn (SetOpt ShowOrigErr) = do process stdout fn (SetOpt ShowOrigErr)
                                             iPrintResult ""
ideslaveProcess fn (UnsetOpt ShowOrigErr) = do process stdout fn (UnsetOpt ShowOrigErr)
                                               iPrintResult ""
ideslaveProcess fn (SetOpt x) = process stdout fn (SetOpt x)
ideslaveProcess fn (UnsetOpt x) = process stdout fn (UnsetOpt x)
ideslaveProcess fn (CaseSplitAt False pos str) = process stdout fn (CaseSplitAt False pos str)
ideslaveProcess fn (AddProofClauseFrom False pos str) = process stdout fn (AddProofClauseFrom False pos str)
ideslaveProcess fn (AddClauseFrom False pos str) = process stdout fn (AddClauseFrom False pos str)
ideslaveProcess fn (AddMissing False pos str) = process stdout fn (AddMissing False pos str)
ideslaveProcess fn (MakeWith False pos str) = process stdout fn (MakeWith False pos str)
ideslaveProcess fn (DoProofSearch False r pos str xs) = process stdout fn (DoProofSearch False r pos str xs)
ideslaveProcess fn (SetConsoleWidth w) = do process stdout fn (SetConsoleWidth w)
                                            iPrintResult ""
ideslaveProcess fn (Apropos a) = do process stdout fn (Apropos a)
                                    iPrintResult ""
ideslaveProcess fn (WhoCalls n) = process stdout fn (WhoCalls n)
ideslaveProcess fn (CallsWho n) = process stdout fn (CallsWho n)
ideslaveProcess fn _ = iPrintError "command not recognized or not supported"


-- | The prompt consists of the currently loaded modules, or "Idris" if there are none
mkPrompt [] = "Idris"
mkPrompt [x] = "*" ++ dropExtension x
mkPrompt (x:xs) = "*" ++ dropExtension x ++ " " ++ mkPrompt xs

-- | Determine whether a file uses literate syntax
lit f = case splitExtension f of
            (_, ".lidr") -> True
            _ -> False

processInput :: String ->
                IState -> [FilePath] -> Idris (Maybe [FilePath])
processInput cmd orig inputs
    = do i <- getIState
         let opts = idris_options i
         let quiet = opt_quiet opts
         let fn = case inputs of
                        (f:_) -> f
                        _ -> ""
         c <- colourise
         case parseCmd i "(input)" cmd of
            Failure err ->   do runIO $ print (fixColour c err)
                                return (Just inputs)
            Success Reload ->
                do putIState $ orig { idris_options = idris_options i
                                    , idris_colourTheme = idris_colourTheme i
                                    }
                   clearErr
                   mods <- loadInputs stdout inputs Nothing
                   return (Just inputs)
            Success (Load f toline) ->
                do putIState orig { idris_options = idris_options i
                                  , idris_colourTheme = idris_colourTheme i
                                  }
                   clearErr
                   mod <- loadInputs stdout [f] toline
                   return (Just [f])
            Success (ModImport f) ->
                do clearErr
                   fmod <- loadModule stdout f
                   return (Just (inputs ++ [fmod]))
            Success Edit -> do -- takeMVar stvar
                               edit fn orig
                               return (Just inputs)
            Success Proofs -> do proofs orig
                                 return (Just inputs)
            Success Quit -> do when (not quiet) (iputStrLn "Bye bye")
                               return Nothing
            Success cmd  -> do
                liftIO $ print $ encode cmd
                idrisCatch (process stdout fn cmd)
                           (showErr >=> iputStrLn)
                return (Just inputs)

resolveProof :: Name -> Idris Name
resolveProof n'
  = do i <- getIState
       ctxt <- getContext
       n <- case lookupNames n' ctxt of
                 [x] -> return x
                 [] -> return n'
                 ns -> ierror (CantResolveAlts (map show ns))
       return n

removeProof :: Name -> Idris ()
removeProof n =
  do i <- getIState
     let proofs = proof_list i
     let ps = filter ((/= n) . fst) proofs
     putIState $ i { proof_list = ps }

edit :: FilePath -> IState -> Idris ()
edit "" orig = iputStrLn "Nothing to edit"
edit f orig
    = do i <- getIState
         env <- runIO $ getEnvironment
         let editor = getEditor env
         let line = case errSpan i of
                        Just l -> " +" ++ show (fst (fc_start l)) ++ " "
                        Nothing -> " "
         let cmd = editor ++ line ++ fixName f
         runIO $ system cmd
         clearErr
         putIState $ orig { idris_options = idris_options i
                          , idris_colourTheme = idris_colourTheme i
                          }
         loadInputs stdout [f] Nothing
--          clearOrigPats
         iucheck
         return ()
   where getEditor env | Just ed <- lookup "EDITOR" env = ed
                       | Just ed <- lookup "VISUAL" env = ed
                       | otherwise = "vi"
         fixName file | map toLower (takeExtension file) `elem` [".lidr", ".idr"] = file
                      | otherwise = addExtension file "idr"



proofs :: IState -> Idris ()
proofs orig
  = do i <- getIState
       let ps = proof_list i
       case ps of
            [] -> iputStrLn "No proofs available"
            _  -> iputStrLn $ "Proofs:\n\t" ++ (show $ map fst ps)

insertScript :: String -> [String] -> [String]
insertScript prf [] = "\n---------- Proofs ----------" : "" : [prf]
insertScript prf (p@"---------- Proofs ----------" : "" : xs)
    = p : "" : prf : xs
insertScript prf (x : xs) = x : insertScript prf xs

process :: IO.Handle -> FilePath -> Command -> Idris ()
process h fn Help = iPrintResult displayHelp
process h fn (ChangeDirectory f)
                 = do runIO $ setCurrentDirectory f
                      return ()
process h fn (Eval t) = withErrorReflection $ do
    logLvl 5 $ show t
    liftIO $ putStrLn "0"
    (tm, ty) <- elabVal toplevel False t
    liftIO $ putStrLn "1"
    ctxt <- getContext
    liftIO $ putStrLn "2"
    let tm' = force (normaliseAll ctxt [] tm)
        ty' = force (normaliseAll ctxt [] ty)
    -- Add value to context, call it "it"
    updateContext (addCtxtDef (sUN "it") (Function ty' tm'))
    liftIO $ putStrLn "3"
    ist <- getIState
    liftIO $ putStrLn "4"
    logLvl 3 $ "Raw: " ++ show (tm', ty')
    logLvl 10 $ "Debug: " ++ showEnvDbg [] tm'
    runIO $ B8.hPutStrLn h $ encode (tm, ty)
    -- let tmDoc = prettyIst ist (delab ist tm')
    -- tyDoc = prettyIst ist (delab ist ty')
    -- ihPrintTermWithType h tmDoc tyDoc

process h fn (ExecVal t)
                  = do ctxt <- getContext
                       ist <- getIState
                       (tm, ty) <- elabVal toplevel False t
--                       let tm' = normaliseAll ctxt [] tm
                       let ty' = normaliseAll ctxt [] ty
                       res <- execute tm
                       let (resOut, tyOut) = (prettyIst ist (delab ist res),
                                              prettyIst ist (delab ist ty'))
                       ihPrintTermWithType h resOut tyOut

process h fn (Check (PRef _ n))
   = do ctxt <- getContext
        ist <- getIState
        let ppo = ppOptionIst ist
        case lookupNames n ctxt of
          ts@(t:_) ->
            case lookup t (idris_metavars ist) of
                Just (_, i, _) -> ihRenderResult h . fmap (fancifyAnnots ist) $
                                  showMetavarInfo ppo ist n i
                Nothing -> ihPrintFunTypes h [] n (map (\n -> (n, delabTy ist n)) ts)
          [] -> ihPrintError h $ "No such variable " ++ show n
  where
    showMetavarInfo ppo ist n i
         = case lookupTy n (tt_ctxt ist) of
                (ty:_) -> putTy ppo ist i [] (delab ist (errReverse ist ty))
    putTy :: PPOption -> IState -> Int -> [(Name, Bool)] -> PTerm -> Doc OutputAnnotation
    putTy ppo ist 0 bnd sc = putGoal ppo ist bnd sc
    putTy ppo ist i bnd (PPi _ n t sc)
               = let current = text "  " <>
                               (case n of
                                   MN _ _ -> text "_"
                                   UN nm | ('_':'_':_) <- str nm -> text "_"
                                   _ -> bindingOf n False) <+>
                               colon <+> align (tPretty bnd ist t) <> line
                 in
                    current <> putTy ppo ist (i-1) ((n,False):bnd) sc
    putTy ppo ist _ bnd sc = putGoal ppo ist ((n,False):bnd) sc
    putGoal ppo ist bnd g
               = text "--------------------------------------" <$>
                 annotate (AnnName n Nothing Nothing Nothing) (text $ show n) <+> colon <+>
                 align (tPretty bnd ist g)

    tPretty bnd ist t = pprintPTerm (ppOptionIst ist) bnd [] (idris_infixes ist) t


process h fn (Check t)
   = do (tm, ty) <- elabVal toplevel False t
        ctxt <- getContext
        ist <- getIState
        let ppo = ppOptionIst ist
            ty' = normaliseC ctxt [] ty
        case tm of
           TType _ ->
             ihPrintTermWithType h (prettyIst ist PType) type1Doc
           _ -> ihPrintTermWithType h (prettyIst ist (delab ist tm))
                                      (prettyIst ist (delab ist ty))

process h fn (DocStr (Left n))
   = do ist <- getIState
        case lookupCtxtName n (idris_docstrings ist) of
          [] -> iPrintError $ "No documentation for " ++ show n
          ns -> do toShow <- mapM (showDoc ist) ns
                   ihRenderResult h (vsep toShow)
    where showDoc ist (n, d) = do doc <- getDocs n
                                  return $ pprintDocs ist doc

process h fn (DocStr (Right c))
   = do ist <- getIState
        ihRenderResult h $ pprintConstDocs ist c (constDocs c)

process h fn Universes
                     = do i <- getIState
                          let cs = idris_constraints i
--                        iputStrLn $ showSep "\n" (map show cs)
                          iputStrLn $ show (map fst cs)
                          let n = length cs
                          iputStrLn $ "(" ++ show n ++ " constraints)"
                          case ucheck cs of
                            Error e -> iPrintError $ pshow i e
                            OK _ -> iPrintResult "Universes OK"
process h fn (Defn n)
                    = do i <- getIState
                         iputStrLn "Compiled patterns:\n"
                         iputStrLn $ show (lookupDef n (tt_ctxt i))
                         case lookupCtxt n (idris_patdefs i) of
                            [] -> return ()
                            [(d, _)] -> do iputStrLn "Original definiton:\n"
                                           mapM_ (printCase i) d
                         case lookupTotal n (tt_ctxt i) of
                            [t] -> iputStrLn (showTotal t i)
                            _ -> return ()
    where printCase i (_, lhs, rhs)
             = let i' = i { idris_options = (idris_options i) { opt_showimp = True } }
               in iputStrLn (showTm i' (delab i lhs) ++ " = " ++
                             showTm i' (delab i rhs))
process h fn (TotCheck n)
                        = do i <- getIState
                             case lookupNameTotal n (tt_ctxt i) of
                                []  -> ihPrintError h $ "Unknown operator " ++ show n
                                ts  -> do ist <- getIState
                                          c <- colourise
                                          let ppo =  ppOptionIst ist
                                          let showN = showName (Just ist) [] ppo c
                                          ihPrintResult h . concat . intersperse "\n" .
                                            map (\(n, t) -> showN n ++ " is " ++ showTotal t i) $
                                            ts


process h fn (DebugInfo n)
   = do i <- getIState
        let oi = lookupCtxtName n (idris_optimisation i)
        when (not (null oi)) $ iputStrLn (show oi)
        let si = lookupCtxt n (idris_statics i)
        when (not (null si)) $ iputStrLn (show si)
        let di = lookupCtxt n (idris_datatypes i)
        when (not (null di)) $ iputStrLn (show di)
        let d = lookupDef n (tt_ctxt i)
        when (not (null d)) $ iputStrLn $ "Definition: " ++ (show (head d))
        let cg = lookupCtxtName n (idris_callgraph i)
        i <- getIState
        let cg' = lookupCtxtName n (idris_callgraph i)
        sc <- checkSizeChange n
        iputStrLn $ "Size change: " ++ show sc
        when (not (null cg')) $ do iputStrLn "Call graph:\n"
                                   iputStrLn (show cg')
process h fn (Search t) = searchByType h (searchPred defaultScoreFunction) scoreLimit t where
  scoreLimit = 100
process h fn (CaseSplitAt updatefile l n)
    = caseSplitAt h fn updatefile l n
process h fn (AddClauseFrom updatefile l n)
    = addClauseFrom h fn updatefile l n
process h fn (AddProofClauseFrom updatefile l n)
    = addProofClauseFrom h fn updatefile l n
process h fn (AddMissing updatefile l n)
    = addMissing h fn updatefile l n
process h fn (MakeWith updatefile l n)
    = makeWith h fn updatefile l n
process h fn (MakeLemma updatefile l n)
    = makeLemma h fn updatefile l n
process h fn (DoProofSearch updatefile rec l n hints)
    = doProofSearch h fn updatefile rec l n hints Nothing
process h fn (Spec t)
                    = do (tm, ty) <- elabVal toplevel False t
                         ctxt <- getContext
                         ist <- getIState
                         let tm' = simplify ctxt [] {- (idris_statics ist) -} tm
                         iPrintResult (show (delab ist tm'))

process h fn (RmProof n')
  = do i <- getIState
       n <- resolveProof n'
       let proofs = proof_list i
       case lookup n proofs of
            Nothing -> iputStrLn "No proof to remove"
            Just _  -> do removeProof n
                          insertMetavar n
                          iputStrLn $ "Removed proof " ++ show n
                          where
                            insertMetavar :: Name -> Idris ()
                            insertMetavar n =
                              do i <- getIState
                                 let ms = idris_metavars i
                                 putIState $ i { idris_metavars = (n, (Nothing, 0, False)) : ms }

process h fn' (AddProof prf)
  = do fn <- do
         let fn'' = takeWhile (/= ' ') fn'
         ex <- runIO $ doesFileExist fn''
         let fnExt = fn'' <.> "idr"
         exExt <- runIO $ doesFileExist fnExt
         if ex
            then return fn''
            else if exExt
                    then return fnExt
                    else ifail $ "Neither \""++fn''++"\" nor \""++fnExt++"\" exist"
       let fb = fn ++ "~"
       runIO $ copyFile fn fb -- make a backup in case something goes wrong!
       prog <- runIO $ readFile fb
       i <- getIState
       let proofs = proof_list i
       n' <- case prf of
                Nothing -> case proofs of
                             [] -> ifail "No proof to add"
                             ((x, p) : _) -> return x
                Just nm -> return nm
       n <- resolveProof n'
       case lookup n proofs of
            Nothing -> iputStrLn "No proof to add"
            Just p  -> do let prog' = insertScript (showProof (lit fn) n p) ls
                          runIO $ writeFile fn (unlines prog')
                          removeProof n
                          iputStrLn $ "Added proof " ++ show n
                          where ls = (lines prog)

process h fn (ShowProof n')
  = do i <- getIState
       n <- resolveProof n'
       let proofs = proof_list i
       case lookup n proofs of
            Nothing -> iPrintError "No proof to show"
            Just p  -> iPrintResult $ showProof False n p

process h fn (Prove n')
     = do ctxt <- getContext
          ist <- getIState
          let ns = lookupNames n' ctxt
          let metavars = mapMaybe (\n -> do c <- lookup n (idris_metavars ist); return (n, c)) ns
          n <- case metavars of
              [] -> ierror (Msg $ "Cannot find metavariable " ++ show n')
              [(n, (_,_,False))] -> return n
              [(_, (_,_,True))]  -> ierror (Msg $ "Declarations not solvable using prover")
              ns -> ierror (CantResolveAlts (map show ns))
          prover (lit fn) n
          -- recheck totality
          i <- getIState
          totcheck (fileFC "(input)", n)
          mapM_ (\ (f,n) -> setTotality n Unchecked) (idris_totcheck i)
          mapM_ checkDeclTotality (idris_totcheck i)
          warnTotality

process h fn (HNF t)
                    = do (tm, ty) <- elabVal toplevel False t
                         ctxt <- getContext
                         ist <- getIState
                         let tm' = hnf ctxt [] tm
                         iPrintResult (show (delab ist tm'))
process h fn (TestInline t)
                           = do (tm, ty) <- elabVal toplevel False t
                                ctxt <- getContext
                                ist <- getIState
                                let tm' = inlineTerm ist tm
                                c <- colourise
                                iPrintResult (showTm ist (delab ist tm'))
process h fn Execute
                   = idrisCatch
                       (do ist <- getIState
                           (m, _) <- elabVal toplevel False
                                           (PApp fc
                                              (PRef fc (sUN "run__IO"))
                                              [pexp $ PRef fc (sNS (sUN "main") ["Main"])])
                           (tmpn, tmph) <- runIO tempfile
                           runIO $ hClose tmph
                           t <- codegen
                           compile t tmpn m
                           case idris_outputmode ist of
                             RawOutput -> do runIO $ system tmpn
                                             return ()
                             IdeSlave n -> runIO . hPutStrLn h $
                                           IdeSlave.convSExp "run-program" tmpn n)
                       (\e -> getIState >>= ihRenderError stdout . flip pprintErr e)
  where fc = fileFC "main"
process h fn (Compile codegen f)
      = do (m, _) <- elabVal toplevel False
                       (PApp fc (PRef fc (sUN "run__IO"))
                       [pexp $ PRef fc (sNS (sUN "main") ["Main"])])
           compile codegen f m
  where fc = fileFC "main"
process h fn (LogLvl i) = setLogLevel i
-- Elaborate as if LHS of a pattern (debug command)
process h fn (Pattelab t)
     = do (tm, ty) <- elabVal toplevel True t
          iPrintResult $ show tm ++ "\n\n : " ++ show ty

process h fn (Missing n)
    = do i <- getIState
         let i' = i { idris_options = (idris_options i) { opt_showimp = True } }
         case lookupCtxt n (idris_patdefs i) of
                  [] -> ihPrintError h $ "Unknown operator " ++ show n
                  [(_, tms)] ->
                       iPrintResult (showSep "\n" (map (showTm i') tms))
                  _ -> iPrintError $ "Ambiguous name"
process h fn (DynamicLink l)
                           = do i <- getIState
                                let importdirs = opt_importdirs (idris_options i)
                                    lib = trim l
                                handle <- lift . lift $ tryLoadLib importdirs lib
                                case handle of
                                  Nothing -> iPrintError $ "Could not load dynamic lib \"" ++ l ++ "\""
                                  Just x -> do let libs = idris_dynamic_libs i
                                               if x `elem` libs
                                                  then do iLOG ("Tried to load duplicate library " ++ lib_name x)
                                                          return ()
                                                  else putIState $ i { idris_dynamic_libs = x:libs }
    where trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
process h fn ListDynamic
                       = do i <- getIState
                            iputStrLn "Dynamic libraries:"
                            showLibs $ idris_dynamic_libs i
    where showLibs []                = return ()
          showLibs ((Lib name _):ls) = do iputStrLn $ "\t" ++ name; showLibs ls
process h fn Metavars
                 = do ist <- getIState
                      let mvs = map fst (idris_metavars ist) \\ primDefs
                      case mvs of
                        [] -> iPrintError "No global metavariables to solve"
                        _ -> iPrintResult $ "Global metavariables:\n\t" ++ show mvs
process h fn NOP      = return ()

process h fn (SetOpt   ErrContext)  = setErrContext True
process h fn (UnsetOpt ErrContext)  = setErrContext False
process h fn (SetOpt ShowImpl)      = setImpShow True
process h fn (UnsetOpt ShowImpl)    = setImpShow False
process h fn (SetOpt ShowOrigErr)   = setShowOrigErr True
process h fn (UnsetOpt ShowOrigErr) = setShowOrigErr False
process h fn (SetOpt AutoSolve)     = setAutoSolve True
process h fn (UnsetOpt AutoSolve)   = setAutoSolve False
process h fn (SetOpt NoBanner)      = setNoBanner True
process h fn (UnsetOpt NoBanner)    = setNoBanner False
process h fn (SetOpt WarnReach)     = fmodifyState opts_idrisCmdline $ nub . (WarnReach:)
process h fn (UnsetOpt WarnReach)   = fmodifyState opts_idrisCmdline $ delete WarnReach

process h fn (SetOpt _) = iPrintError "Not a valid option"
process h fn (UnsetOpt _) = iPrintError "Not a valid option"
process h fn (SetColour ty c) = setColour ty c
process h fn ColourOn
                    = do ist <- getIState
                         putIState $ ist { idris_colourRepl = True }
process h fn ColourOff
                     = do ist <- getIState
                          putIState $ ist { idris_colourRepl = False }
process h fn ListErrorHandlers =
  do ist <- getIState
     case idris_errorhandlers ist of
       [] -> iPrintResult "No registered error handlers"
       handlers ->
           iPrintResult $ "Registered error handlers: " ++ (concat . intersperse ", " . map show) handlers
process h fn (SetConsoleWidth w) = setWidth w

process h fn (Apropos a) =
  do ist <- getIState
     let names = apropos ist (T.pack a)
     let aproposInfo = [ (n,
                          delabTy ist n,
                          fmap (overview . fst) (lookupCtxtExact n (idris_docstrings ist)))
                       | n <- sort names, isUN n ]
     ihRenderResult h $ vsep (map (renderApropos ist) aproposInfo)
  where renderApropos ist (name, ty, docs) =
          prettyName True [] name <+> colon <+> align (prettyIst ist ty) <$>
          fromMaybe empty (fmap (\d -> renderDocstring d <> line) docs)
        isUN (UN _) = True
        isUN (NS n _) = isUN n
        isUN _ = False

process h fn (WhoCalls n) =
  do calls <- whoCalls n
     ist <- getIState
     ihRenderResult h . vsep $
       map (\(n, ns) ->
             text "Callers of" <+> prettyName True [] n <$>
             indent 1 (vsep (map ((text "*" <+>) . align . prettyName True []) ns)))
           calls

process h fn (CallsWho n) =
  do calls <- callsWho n
     ist <- getIState
     ihRenderResult h . vsep $
       map (\(n, ns) ->
             prettyName True [] n <+> text "calls:" <$>
             indent 1 (vsep (map ((text "*" <+>) . align . prettyName True []) ns)))
           calls
-- IdrisDoc
process h fn (MakeDoc s) =
  do     istate        <- getIState
         let names      = words s
             parse n    | Success x <- runparser name istate fn n = Right x
             parse n    = Left n
             (bad, nss) = partitionEithers $ map parse names
         cd            <- runIO $ getCurrentDirectory
         let outputDir  = cd </> "doc"
         result        <- if null bad then runIO $ generateDocs istate nss outputDir
                                      else return . Left $ "Illegal name: " ++ head bad
         case result of Right _   -> iputStrLn "IdrisDoc generated"
                        Left  err -> iPrintError err


showTotal :: Totality -> IState -> String
showTotal t@(Partial (Other ns)) i
   = show t ++ "\n\t" ++ showSep "\n\t" (map (showTotalN i) ns)
showTotal t i = show t
showTotalN i n = case lookupTotal n (tt_ctxt i) of
                        [t] -> showTotal t i
                        _ -> ""

displayHelp = let vstr = showVersion version in
              "\nIdris version " ++ vstr ++ "\n" ++
              "--------------" ++ map (\x -> '-') vstr ++ "\n\n" ++
              concatMap cmdInfo helphead ++
              concatMap cmdInfo help
  where cmdInfo (cmds, args, text) = "   " ++ col 16 12 (showSep " " cmds) (show args) text
        col c1 c2 l m r =
            l ++ take (c1 - length l) (repeat ' ') ++
            m ++ take (c2 - length m) (repeat ' ') ++ r ++ "\n"

helphead =
  [ (["Command"], SpecialHeaderArg, "Purpose"),
    ([""], NoArg, "")
  ]


replSettings :: Maybe FilePath -> H.Settings Idris
replSettings hFile = setComplete replCompletion $ H.defaultSettings {
                       historyFile = hFile
                     }

-- | Invoke as if from command line. It is an error if there are unresolved totality problems.
idris :: [Opt] -> IO (Maybe IState)
idris opts = do res <- runErrorT $ execStateT totalMain idrisInit
                case res of
                  Left err -> do putStrLn $ pshow idrisInit err
                                 return Nothing
                  Right ist -> return (Just ist)
    where totalMain = do idrisMain opts
                         ist <- getIState
                         case idris_totcheckfail ist of
                           ((fc, msg):_) -> ierror . At fc . Msg $ "Could not build: "++  msg
                           [] -> return ()


loadInputs :: IO.Handle -> [FilePath] -> Maybe Int -> Idris ()
loadInputs h inputs toline -- furthest line to read in input source files
  = idrisCatch
       (do ist <- getIState
           -- if we're in --check and not outputting anything, don't bother
           -- loading, as it gets really slow if there's lots of modules in
           -- a package (instead, reload all at the end to check for
           -- consistency only)
           opts <- getCmdLine

           let loadCode = case opt getOutput opts of
                               [] -> not (NoREPL `elem` opts)
                               _ -> True

           -- For each ifile list, check it and build ibcs in the same clean IState
           -- so that they don't interfere with each other when checking

           let ninputs = zip [1..] inputs
           ifiles <- mapWhileOK (\(num, input) ->
                do putIState ist
                   modTree <- buildTree
                                   (map snd (take (num-1) ninputs))
                                   input
                   let ifiles = getModuleFiles modTree
                   iLOG ("MODULE TREE : " ++ show modTree)
                   iLOG ("RELOAD: " ++ show ifiles)
                   when (not (all ibc ifiles) || loadCode) $
                        tryLoad False (filter (not . ibc) ifiles)
                   -- return the files that need rechecking
                   return ifiles)
                      ninputs
           inew <- getIState
           let tidata = idris_tyinfodata inew
           let patdefs = idris_patdefs inew
           -- If it worked, load the whole thing from all the ibcs together
           case errSpan inew of
              Nothing ->
                do putIState (ist { idris_tyinfodata = tidata })
                   ibcfiles <- mapM findNewIBC (nub (concat ifiles))
                   tryLoad True (mapMaybe id ibcfiles)
              _ -> return ()
           ist <- getIState
           putIState (ist { idris_tyinfodata = tidata,
                            idris_patdefs = patdefs })

           case opt getOutput opts of
               [] -> performUsageAnalysis  -- interactive
               _  -> return []  -- batch, will be checked by the compiler

           return ())
        (\e -> do i <- getIState
                  case e of
                    At f e' -> do setErrSpan f
                                  ihWarn stdout f $ pprintErr i e'
                    ProgramLineComment -> return () -- fail elsewhere
                    _ -> do setErrSpan emptyFC -- FIXME! Propagate it
                            ihWarn stdout emptyFC $ pprintErr i e)
   where -- load all files, stop if any fail
         tryLoad :: Bool -> [IFileType] -> Idris ()
         tryLoad keepstate [] = warnTotality >> return ()
         tryLoad keepstate (f : fs)
                 = do ist <- getIState
                      let maxline
                            = case toline of
                                Nothing -> Nothing
                                Just l -> case f of
                                            IDR fn -> if any (fmatch fn) inputs
                                                         then Just l
                                                         else Nothing
                                            LIDR fn -> if any (fmatch fn) inputs
                                                          then Just l
                                                          else Nothing
                                            _ -> Nothing
                      loadFromIFile h f maxline
                      inew <- getIState
                      -- FIXME: Save these in IBC to avoid this hack! Need to
                      -- preserve it all from source inputs
                      let tidata = idris_tyinfodata inew
                      let patdefs = idris_patdefs inew
                      ok <- noErrors
                      when ok $ do when (not keepstate) $ putIState ist
                                   ist <- getIState
                                   putIState (ist { idris_tyinfodata = tidata,
                                                    idris_patdefs = patdefs })
                                   tryLoad keepstate fs

         ibc (IBC _ _) = True
         ibc _ = False

         fmatch ('.':'/':xs) ys = fmatch xs ys
         fmatch xs ('.':'/':ys) = fmatch xs ys
         fmatch xs ys = xs == ys

         findNewIBC :: IFileType -> Idris (Maybe IFileType)
         findNewIBC i@(IBC _ _) = return (Just i)
         findNewIBC s@(IDR f) = do ist <- get
                                   ibcsd <- valIBCSubDir ist
                                   let ibc = ibcPathNoFallback ibcsd f
                                   ok <- runIO $ doesFileExist ibc
                                   if ok then return (Just (IBC ibc s))
                                         else return Nothing
         findNewIBC s@(LIDR f) = do ist <- get
                                    ibcsd <- valIBCSubDir ist
                                    let ibc = ibcPathNoFallback ibcsd f
                                    ok <- runIO $ doesFileExist ibc
                                    if ok then return (Just (IBC ibc s))
                                          else return Nothing

         -- Like mapM, but give up when there's an error
         mapWhileOK f [] = return []
         mapWhileOK f (x : xs) = do x' <- f x
                                    ok <- noErrors
                                    if ok then do xs' <- mapWhileOK f xs
                                                  return (x' : xs')
                                          else return [x']

idrisMain :: [Opt] -> Idris ()
idrisMain opts =
    do let inputs = opt getFile opts
       liftIO $ do
           putStrLn "inputs:"
           print inputs
       let quiet = Quiet `elem` opts
       let nobanner = NoBanner `elem` opts
       let idesl = Ideslave `elem` opts
       let server = IsServer `elem` opts
       let runrepl = not (NoREPL `elem` opts)
       let verbose = runrepl || Verbose `elem` opts
       let output = opt getOutput opts
       let ibcsubdir = opt getIBCSubDir opts
       let importdirs = opt getImportDir opts
       let bcs = opt getBC opts
       let pkgdirs = opt getPkgDir opts
       let optimize = case opt getOptLevel opts of
                        [] -> 2
                        xs -> last xs
       trpl <- case opt getTriple opts of
                 [] -> runIO $ getDefaultTargetTriple
                 xs -> return (last xs)
       tcpu <- case opt getCPU opts of
                 [] -> runIO $ getHostCPUName
                 xs -> return (last xs)
       let outty = case opt getOutputTy opts of
                     [] -> Executable
                     xs -> last xs
       let cgn = case opt getCodegen opts of
                   [] -> ViaC
                   xs -> last xs
       script <- case opt getExecScript opts of
                   []     -> return Nothing
                   x:y:xs -> do iputStrLn "More than one interpreter expression found."
                                runIO $ exitWith (ExitFailure 1)
                   [expr] -> return (Just expr)
       let immediate = opt getEvalExpr opts

       when (DefaultTotal `elem` opts) $ do i <- getIState
                                            putIState (i { default_total = True })
       setColourise $ not quiet && last (True : opt getColour opts)
       when (not runrepl) $ setWidth InfinitelyWide
       mapM_ addLangExt (opt getLanguageExt opts)
       setREPL runrepl
       setQuiet (quiet || isJust script || not (null immediate))
       setIdeSlave idesl
       setServer server
       setVerbose verbose
       setCmdLine opts
       setOutputTy outty
       setNoBanner nobanner
       setCodegen cgn
       setTargetTriple trpl
       setTargetCPU tcpu
       setOptLevel optimize
       mapM_ makeOption opts
       -- if we have the --bytecode flag, drop into the bytecode assembler
       case bcs of
         [] -> return ()
         xs -> return () -- runIO $ mapM_ bcAsm xs
       case ibcsubdir of
         [] -> setIBCSubDir ""
         (d:_) -> setIBCSubDir d
       setImportDirs importdirs

       setNoBanner nobanner

       when (not (NoBasePkgs `elem` opts)) $ do
           addPkgDir "prelude"
           addPkgDir "base"
       mapM_ addPkgDir pkgdirs
       elabPrims
       when (not (NoBuiltins `elem` opts)) $ do x <- loadModule stdout "Builtins"
                                                return ()
       when (not (NoPrelude `elem` opts)) $ do x <- loadModule stdout "Prelude"
                                               return ()

       when (runrepl && not idesl && not server) initScript

       nobanner <- getNoBanner

       when (runrepl &&
             not quiet &&
             not idesl &&
             not server &&
             not (isJust script) &&
             not nobanner &&
             null immediate) $
         iputStrLn banner

       orig <- getIState
       loadInputs stdout inputs Nothing

       runIO $ hSetBuffering stdout LineBuffering

       ok <- noErrors
       when ok $ case output of
                    [] -> return ()
                    (o:_) -> idrisCatch (process stdout "" (Compile cgn o))
                               (\e -> do ist <- getIState ; iputStrLn $ pshow ist e)

       case immediate of
         [] -> return ()
         exprs -> do setWidth InfinitelyWide
                     mapM_ (\str -> do ist <- getIState
                                       c <- colourise
                                       case parseExpr ist str of
                                         Failure err -> do iputStrLn $ show (fixColour c err)
                                                           runIO $ exitWith (ExitFailure 1)
                                         Success e -> process stdout "" (Eval e))
                           exprs
                     runIO $ exitWith ExitSuccess


       case script of
         Nothing -> return ()
         Just expr -> execScript expr

       -- Create Idris data dir + repl history and config dir
       idrisCatch (do dir <- getIdrisUserDataDir
                      exists <- runIO $ doesDirectoryExist dir
                      unless exists $ iLOG ("Creating " ++ dir)
                      runIO $ createDirectoryIfMissing True (dir </> "repl"))
         (\e -> return ())

       historyFile <- fmap (</> "repl" </> "history") getIdrisUserDataDir

       {-
       liftIO $ do
           putStrLn "tt_ctxt"
           print $ tt_ctxt orig
           putStrLn "idris_constraints"
           print $ idris_constraints orig
           putStrLn "idris_docstrings"
           print $ idris_docstrings orig
           putStrLn "idris_classes"
           print $ idris_classes orig
           putStrLn "idris_implicits"
           print $ idris_implicits orig
           putStrLn "idris_callgraph"
           print $ idris_callgraph orig
           putStrLn "idris_calledgraph"
           print $ idris_calledgraph orig
           putStrLn "idris_tyinfodata"
           print $ idris_tyinfodata orig
           putStrLn "idris_whocalls"
           print $ idris_whocalls orig
           putStrLn "idris_callswho"
           print $ idris_callswho orig
           -}
       when (runrepl && not idesl && not server) $ do
--          clearOrigPats
         startClientServer orig inputs
         startJsonServer orig inputs
         liftIO $ startCoreServer orig inputs
         runInputT (replSettings (Just historyFile)) $ repl orig inputs
       when (idesl || server) $ startDependent orig inputs
       ok <- noErrors
       when (not ok) $ runIO (exitWith (ExitFailure 1))
  where
    makeOption (OLogging i) = setLogLevel i
    makeOption TypeCase = setTypeCase True
    makeOption TypeInType = setTypeInType True
    makeOption NoCoverage = setCoverage False
    makeOption ErrContext = setErrContext True
    makeOption _ = return ()

    addPkgDir :: String -> Idris ()
    addPkgDir p = do ddir <- runIO $ getDataDir
                     addImportDir (ddir </> p)

execScript :: String -> Idris ()
execScript expr = do i <- getIState
                     c <- colourise
                     case parseExpr i expr of
                          Failure err -> do iputStrLn $ show (fixColour c err)
                                            runIO $ exitWith (ExitFailure 1)
                          Success term -> do ctxt <- getContext
                                             (tm, _) <- elabVal toplevel False term
                                             res <- execute tm
                                             runIO $ exitWith ExitSuccess

-- | Get the platform-specific, user-specific Idris dir
getIdrisUserDataDir :: Idris FilePath
getIdrisUserDataDir = runIO $ getAppUserDataDirectory "idris"

-- | Locate the platform-specific location for the init script
getInitScript :: Idris FilePath
getInitScript = do idrisDir <- getIdrisUserDataDir
                   return $ idrisDir </> "repl" </> "init"

-- | Run the initialisation script
initScript :: Idris ()
initScript = do script <- getInitScript
                idrisCatch (do go <- runIO $ doesFileExist script
                               when go $ do
                                 h <- runIO $ openFile script ReadMode
                                 runInit h
                                 runIO $ hClose h)
                           (\e -> iPrintError $ "Error reading init file: " ++ show e)
    where runInit :: IO.Handle -> Idris ()
          runInit h = do eof <- lift . lift $ hIsEOF h
                         ist <- getIState
                         unless eof $ do
                           line <- runIO $ hGetLine h
                           script <- getInitScript
                           c <- colourise
                           processLine ist line script c
                           runInit h
          processLine i cmd input clr =
              case parseCmd i input cmd of
                   Failure err -> runIO $ print (fixColour clr err)
                   Success Reload -> iPrintError "Init scripts cannot reload the file"
                   Success (Load f _) -> iPrintError "Init scripts cannot load files"
                   Success (ModImport f) -> iPrintError "Init scripts cannot import modules"
                   Success Edit -> iPrintError "Init scripts cannot invoke the editor"
                   Success Proofs -> proofs i
                   Success Quit -> iPrintError "Init scripts cannot quit Idris"
                   Success cmd  -> process stdout [] cmd

getFile :: Opt -> Maybe String
getFile (Filename str) = Just str
getFile _ = Nothing

getBC :: Opt -> Maybe String
getBC (BCAsm str) = Just str
getBC _ = Nothing

getOutput :: Opt -> Maybe String
getOutput (Output str) = Just str
getOutput _ = Nothing

getIBCSubDir :: Opt -> Maybe String
getIBCSubDir (IBCSubDir str) = Just str
getIBCSubDir _ = Nothing

getImportDir :: Opt -> Maybe String
getImportDir (ImportDir str) = Just str
getImportDir _ = Nothing

getPkgDir :: Opt -> Maybe String
getPkgDir (Pkg str) = Just str
getPkgDir _ = Nothing

getPkg :: Opt -> Maybe (Bool, String)
getPkg (PkgBuild str) = Just (False, str)
getPkg (PkgInstall str) = Just (True, str)
getPkg _ = Nothing

getPkgClean :: Opt -> Maybe String
getPkgClean (PkgClean str) = Just str
getPkgClean _ = Nothing

getPkgREPL :: Opt -> Maybe String
getPkgREPL (PkgREPL str) = Just str
getPkgREPL _ = Nothing

getPkgCheck :: Opt -> Maybe String
getPkgCheck (PkgCheck str) = Just str
getPkgCheck _              = Nothing

-- | Returns None if given an Opt which is not PkgMkDoc
--   Otherwise returns Just x, where x is the contents of PkgMkDoc
getPkgMkDoc :: Opt          -- ^ Opt to extract
            -> Maybe String -- ^ Result
getPkgMkDoc (PkgMkDoc str) = Just str
getPkgMkDoc _              = Nothing

getCodegen :: Opt -> Maybe Codegen
getCodegen (UseCodegen x) = Just x
getCodegen _ = Nothing

getExecScript :: Opt -> Maybe String
getExecScript (InterpretScript expr) = Just expr
getExecScript _ = Nothing

getEvalExpr :: Opt -> Maybe String
getEvalExpr (EvalExpr expr) = Just expr
getEvalExpr _ = Nothing

getOutputTy :: Opt -> Maybe OutputType
getOutputTy (OutputTy t) = Just t
getOutputTy _ = Nothing

getLanguageExt :: Opt -> Maybe LanguageExt
getLanguageExt (Extension e) = Just e
getLanguageExt _ = Nothing

getTriple :: Opt -> Maybe String
getTriple (TargetTriple x) = Just x
getTriple _ = Nothing

getCPU :: Opt -> Maybe String
getCPU (TargetCPU x) = Just x
getCPU _ = Nothing

getOptLevel :: Opt -> Maybe Word
getOptLevel (OptLevel x) = Just x
getOptLevel _ = Nothing

getColour :: Opt -> Maybe Bool
getColour (ColourREPL b) = Just b
getColour _ = Nothing

opt :: (Opt -> Maybe a) -> [Opt] -> [a]
opt = mapMaybe

ver = showVersion version ++ gitHash

banner = "     ____    __     _                                          \n" ++
         "    /  _/___/ /____(_)____                                     \n" ++
         "    / // __  / ___/ / ___/     Version " ++ ver ++ "\n" ++
         "  _/ // /_/ / /  / (__  )      http://www.idris-lang.org/      \n" ++
         " /___/\\__,_/_/  /_/____/       Type :? for help                \n"


