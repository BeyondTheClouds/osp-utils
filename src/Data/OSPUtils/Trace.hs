{-# LANGUAGE OverloadedStrings #-}
module Data.OSPUtils.Trace where

import Prelude

import Data.Tree
import Data.List (find)
import Control.Applicative ((<|>))
import Control.Monad
import Data.Aeson
import Data.Aeson.Internal (JSONPath, iparse, formatError)
import Data.Aeson.Types (Parser, parse, listParser, typeMismatch)
import Data.Aeson.Parser (decodeWith, eitherDecodeWith, json)
import Data.Text (Text, isSuffixOf)
import qualified Data.HashMap.Lazy as H (lookup, keys)
import qualified Data.ByteString.Lazy.Internal as BLI (ByteString)



-- ADTs
data HTTP = Post | Get | Update | Delete deriving (Show, Eq)

data HTTPReq = HTTPReq
  { path   :: String
  , method :: HTTP
  , query  :: String
  } deriving (Show, Eq)

data DBReq = DBReq
  { stmt   :: String
  , params :: Value
  } deriving (Show, Eq)

data PythonReq = PythonReq
  { function :: String
  , args     :: String
  , kwargs   :: String
  } deriving (Show, Eq)

data (Show a, Eq a) => TraceInfo a = TraceInfo
  { project  :: String
  , service  :: String
  , start    :: String
  , stop     :: Maybe String
  , req      :: a
  } deriving (Show, Eq)


-- | An OSProfiler Trace represented as a Haskell value.
data TraceType = Wsgi (TraceInfo HTTPReq)
               | DB (TraceInfo DBReq)
               | RPC (TraceInfo PythonReq)
               | ComputeApi (TraceInfo PythonReq)
               | NovaImage (TraceInfo PythonReq)
               | NovaVirt (TraceInfo PythonReq)
               | NeutronApi (TraceInfo PythonReq)
               | Root
               deriving (Show, Eq)

type Trace = Tree TraceType


-- Utils

-- | Retrieve the value associated with the path of key of an 'Value'.
-- The result is 'empty' if the path '[Text]' is not present or the
-- value cannot be converted to the desired type.
(.:+) :: (FromJSON a) => Value -> [Text] -> Parser a
(.:+) v t = parseJSON <=< foldM ((maybe err pure .) . lookupE) v $ t
  where
    err :: Parser a
    err = fail $ "No key path for " ++ show t ++ " in " ++ show v

    lookupE :: Value -> Text -> Maybe Value
    lookupE (Object v') key = H.lookup key v'
    lookupE _           _   = Nothing

-- | Retrieve the value associated with the ended key of an 'Object'.
-- The result is 'empty' if there is no key ended by 'Text' or the
-- value cannot be converted to the desired type.
(.:*-) :: (FromJSON a) => Object -> Text -> Parser a
(.:*-) o t = parseJSON <=< ((maybe err pure .) . lookupRE) o $ t
  where
    err :: Parser a
    err = fail $ "No key ended by " ++ show t ++ " in Object"

    lookupRE :: Object -> Text -> Maybe Value
    lookupRE o' suffix = do k <- find (isSuffixOf suffix) (H.keys o')
                            H.lookup k o'

-- | Retrieve the value associated with the ended key of an 'Object'.
-- The result is 'Nothing' if there is no key ended by 'Text' or
-- 'empty' if the value cannot be converted to the desired type.
(.:*-?) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
(.:*-?) o s = (pure . Just <=< (.:*- s)) o <|> pure Nothing


-- | 'Parser' for the json top 'Trace'.
parserTopTrace :: Value -> Parser Trace
parserTopTrace v = Node Root <$> parseChildren v
  where
    parseTrace :: Value -> Parser Trace
    parseTrace v' = Node <$> parseJSON v' <*> parseChildren v'

    parseChildren :: Value -> Parser [Trace]
    parseChildren (Object o') = (listParser parseTrace <=< (.: "children")) o'
    parseChildren v'           = typeMismatch "[a]" v'

class (FromJSON a, Show a, Eq a) => ReqPath a where
  reqPath :: Value -> Parser a

-- ReqPath instances
instance ReqPath HTTPReq where
  reqPath = (.:+ [ "meta.raw_payload.wsgi-start", "info", "request" ])

instance ReqPath DBReq where
  reqPath = (.:+ [ "meta.raw_payload.db-start", "info", "db" ])

instance ReqPath PythonReq where
  reqPath v =
    let rpc        = v .:+ [ "meta.raw_payload.rpc-start",         "info", "function" ]
        computeApi = v .:+ [ "meta.raw_payload.compute_api-start", "info", "function" ]
        novaImage  = v .:+ [ "meta.raw_payload.nova_image-start",  "info", "function" ]
        novaVirt   = v .:+ [ "meta.raw_payload.vif_driver-start",  "info", "function" ]
        neutronApi = v .:+ [ "meta.raw_payload.neutron_api-start", "info", "function" ]
    in rpc <|> computeApi <|> novaImage <|> novaVirt <|> neutronApi

-- FronJSON instances
instance FromJSON HTTP where
  parseJSON (String s) = case s of
    "POST"   -> pure Post
    "GET"    -> pure Get
    "UPDATE" -> pure Update
    "DELETE" -> pure Delete
    _        -> fail $ show s ++ " is not an HTTP verb"
  parseJSON v          = typeMismatch "HTTP Verb" v

instance FromJSON HTTPReq where
  parseJSON (Object o) = HTTPReq <$>
        o .: "path"
    <*> o .: "method"
    <*> o .: "query"
  parseJSON v          = typeMismatch "HTTPReq" v

instance FromJSON DBReq where
  parseJSON (Object o) = DBReq <$>
        o .: "statement"
    <*> o .: "params"
  parseJSON v          = typeMismatch "DBReq" v

instance FromJSON PythonReq where
  parseJSON (Object o) = PythonReq <$>
        o .: "name"
    <*> o .: "args"
    <*> o .: "kwargs"
  parseJSON v          = typeMismatch "PythonReq" v

instance (ReqPath a, FromJSON a) => FromJSON (TraceInfo a) where
  parseJSON v@(Object o) = TraceInfo <$>
        o .: "project"
    <*> o .: "service"
    <*> ((.: "timestamp") <=< (.:*-  "-start")) o
    <*> (maybe (pure Nothing) (.: "timestamp") <=< (.:*-? "-stop"))  o
    <*> reqPath v
  parseJSON v            = typeMismatch "TraceInfo" v

instance FromJSON TraceType where
  parseJSON (Object o)
    =   traceType o "wsgi"        *> (Wsgi       <$> o .: "info")
    <|> traceType o "db"          *> (DB         <$> o .: "info")
    <|> traceType o "rpc"         *> (RPC        <$> o .: "info")
    <|> traceType o "compute_api" *> (ComputeApi <$> o .: "info")
    <|> traceType o "nova_image"  *> (NovaImage  <$> o .: "info")
    <|> traceType o "vif_driver"  *> (NovaVirt   <$> o .: "info")
    <|> traceType o "neutron_api" *> (NeutronApi <$> o .: "info")
    where
      traceType :: Object -> String -> Parser String
      traceType o n = do
        o' <- o  .: "info" -- :: Parser Object
        v' <- o' .: "name" -- :: Parser String
        if v' == n
          then pure v'
          else fail $ show v' ++ " is not a valid TraceType"
  parseJSON v          = typeMismatch "TraceType" v


-- API
decodeTrace :: BLI.ByteString -> Maybe Trace
decodeTrace = decodeWith json (parse parserTopTrace)

eitherDecodeTrace :: BLI.ByteString -> Either String Trace
eitherDecodeTrace = eitherFormatError . eitherDecodeWith json (iparse parserTopTrace)
  where
    eitherFormatError :: Either (JSONPath, String) a -> Either String a
    eitherFormatError = either (Left . uncurry formatError) Right
