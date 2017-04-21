module Data.OSPUtils.Seqdiag where

import Prelude

import Data.OSPUtils.Trace
import Data.OSPUtils.Query

import Data.Maybe
import qualified Data.Tree as T
import qualified Data.ByteString.Lazy as BS (readFile, writeFile)


seqdiag :: Trace -> String
seqdiag t = concat $ seqdiag' serviceName t (T.subForest t)
  where
    seqdiag' :: (TraceType -> String) -> Trace -> [Trace] -> [String]
    seqdiag' f t = map (\t' -> f (T.rootLabel t) ++ " => " ++ f (T.rootLabel t') ++
                         if null (T.subForest t') then ";\n"
                         else " {\n" ++ seqdiag t' ++ "}\n")

    serviceName :: TraceType -> String
    serviceName Root            = "Client"
    serviceName (Wsgi       ti) = project ti ++ "-" ++ service ti ++ "-WSGI"
    serviceName (DB         ti) = project ti ++ "-" ++ service ti ++ "-DB"
    serviceName (RPC        ti) = project ti ++ "-" ++ service ti ++ "-RPC"
    serviceName (ComputeApi ti) = project ti ++ "-" ++ service ti ++ "-ComputeApi"
    serviceName (NovaImage  ti) = project ti ++ "-" ++ service ti ++ "-NovaImage"
    serviceName (NovaVirt   ti) = project ti ++ "-" ++ service ti ++ "-NovaVirt"
    serviceName (NeutronApi ti) = project ti ++ "-" ++ service ti ++ "-NeutronApi"

seqdiagTop :: Trace -> String
seqdiagTop t = "seqdiag {\n" ++ seqdiag t ++ "\n}"

fileToTrace :: String -> IO Trace
fileToTrace fp = do
  json <- BS.readFile fp
  pure $ fromMaybe (T.Node Root []) (decodeTrace json)

main :: IO ()
main = do
  let query = Data.OSPUtils.Query.filter p'
  t <- fileToTrace "tests/rsc/trace-boot-and-delete.yaml.json"
  putStrLn $ T.drawTree $ fmap show (query t)
  writeFile "tests/rsc/out-no-fold.txt" (T.drawTree $ show <$> t)
  writeFile "tests/rsc/out-fold.txt" (T.drawTree $ show <$> query t)
  writeFile "tests/rsc/out.dot" (seqdiagTop $ query t)
  where
    p :: TraceType -> TraceType -> Bool
    -- p (Wsgi _) (Wsgi _) = True
    p (DB   _) (DB   _) = True
    p _        _        = False

    p' :: TraceType -> Bool
    p' (Wsgi _) = True
    p' (RPC _) = True
    p' _ = False
