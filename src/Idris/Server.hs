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

-- pre: prefix?
-- s: expression
convJSON :: SExpable a => String -> a -> String
convJSON pre s = B8.unpack $ BL.toStrict $ encode $ SexpList [SymbolAtom pre, toSExp s]

parseMessage :: String -> Either Err (SExp, Integer)
parseMessage x =
  (\(SexpList [cmd, IntegerAtom id]) -> (cmd, id)) <$> receiveString x

receiveString :: String -> Either Err SExp
receiveString = left Msg . eitherDecode . BL.fromStrict . B8.pack

-- temporary, maybe
type ServerCommand = IdeSlaveCommand
