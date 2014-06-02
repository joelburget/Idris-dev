{-# LANGUAGE OverloadedStrings #-}

module Idris.Server
    ( convJSON
    , parseMessage
    , receiveString
    , ServerCommand
    ) where

import Control.Applicative
import Control.Arrow (left)
import Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H

import Idris.Core.TT
import Idris.IdeSlave (SExp(..), SExpable(..), IdeSlaveCommand)


instance FromJSON SExp where
    parseJSON (Object obj) = case H.lookup "type" obj of
        Just "list"   -> SexpList    <$> obj .: "val"
        Just "string" -> StringAtom  <$> obj .: "val"
        Just "bool"   -> BoolAtom    <$> obj .: "val"
        Just "int"    -> IntegerAtom <$> obj .: "val"
        Just "symb"   -> SymbolAtom  <$> obj .: "val"

instance ToJSON SExp where
    toJSON (SexpList ss)    = object ["type" .= ("list"   :: String), "val" .= ss]
    toJSON (StringAtom str) = object ["type" .= ("string" :: String), "val" .= str]
    toJSON (BoolAtom b)     = object ["type" .= ("bool"   :: String), "val" .= b]
    toJSON (IntegerAtom i)  = object ["type" .= ("int"    :: String), "val" .= i]
    toJSON (SymbolAtom s)   = object ["type" .= ("symb"   :: String), "val" .= s]

-- {"type":"list","val":[{"type":"list","val":[{"type":"symb","val":"interpret"},{"type":"string","val":"2+2"}]},{"type":"int","val":0}]}

-- pre: prefix?
-- s: expression
convJSON :: SExpable a => String -> a -> Integer -> String
convJSON pre s id = B8.unpack $ BL.toStrict $ encode $ SexpList
    [SymbolAtom pre, toSExp s, IntegerAtom id]

parseMessage :: String -> Either Err (SExp, Integer)
parseMessage x =
  (\(SexpList [cmd, IntegerAtom id]) -> (cmd, id)) <$> receiveString x

receiveString :: String -> Either Err SExp
receiveString = left Msg . eitherDecode . BL.fromStrict . B8.pack

-- temporary, maybe
type ServerCommand = IdeSlaveCommand




{-
instance FromJSON Command where
    parseJSON (Object obj) = case H.lookup "cmd" obj of
        Just "Quit" -> pure Quit
        Just "Help" -> pure Help
        Just "Eval" -> Eval <$> obj .: "term"
        Just "DocStr" -> case parseConst orig name of
            Success c -> Right c
            Failure _ -> case splitName name of
               Left err -> fail err
               Right n -> Left n
        Just cmd -> fail $ cmd ++ " not yet supported"
        Nothing -> fail "this is nonsense"

instance ToJSON Command where
    toJSON Quit = object ["cmd" .= "Quit"]
    toJSON Help = object ["cmd" .= "Help"]
    toJSON (Eval term) = object ["cmd" .= "Eval", "term" .= term]

-- | REPL commands
data Command = Quit
             | Help
             | Eval PTerm
             | Check PTerm
             | DocStr (Either Name Const)
             | TotCheck Name
             | Reload
             | Load FilePath (Maybe Int) -- up to maximum line number
             | ChangeDirectory FilePath
             | ModImport String
             | Edit
             | Compile Codegen String
             | Execute
             | ExecVal PTerm
             | Metavars
             | Prove Name
             | AddProof (Maybe Name)
             | RmProof Name
             | ShowProof Name
             | Proofs
             | Universes
             | LogLvl Int
             | Spec PTerm
             | HNF PTerm
             | TestInline PTerm
             | Defn Name
             | Missing Name
             | DynamicLink FilePath
             | ListDynamic
             | Pattelab PTerm
             | DebugInfo Name
             | Search PTerm
             | CaseSplitAt Bool Int Name
             | AddClauseFrom Bool Int Name
             | AddProofClauseFrom Bool Int Name
             | AddMissing Bool Int Name
             | MakeWith Bool Int Name
             | MakeLemma Bool Int Name
             | DoProofSearch Bool Bool Int Name [Name]
               -- ^ the first bool is whether to update,
               -- the second is whether to search recursively (i.e. for the arguments)
             | SetOpt Opt
             | UnsetOpt Opt
             | NOP
             | SetColour ColourType IdrisColour
             | ColourOn
             | ColourOff
             | ListErrorHandlers
             | SetConsoleWidth ConsoleWidth
             | Apropos String
             | WhoCalls Name
             | CallsWho Name
             | MakeDoc String                      -- IdrisDoc
-}
