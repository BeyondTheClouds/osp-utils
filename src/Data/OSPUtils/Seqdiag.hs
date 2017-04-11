module Data.OSPUtils.Seqdiag where

import Prelude

import Data.OSPUtils.Trace

import qualified Data.ByteString.Lazy as BS (readFile, writeFile)


seqdiag :: Trace -> String
seqdiag t = concat $ seqdiag' serviceName t (getChildren t)
  where
    seqdiag' :: (Trace -> String) -> Trace -> [Trace] -> [String]
    seqdiag' f t = map (\t' -> f t ++ " => " ++ f t' ++
                         if null (getChildren t') then ";\n"
                         else " {\n" ++ seqdiag t' ++ "}\n")

    serviceName :: Trace -> String
    serviceName (Root          _) = "Client"
    serviceName (Wsgi       ti _) = project ti ++ "-" ++ service ti ++ "-WSGI"
    serviceName (DB         ti _) = project ti ++ "-" ++ service ti ++ "-DB"
    serviceName (RPC        ti _) = project ti ++ "-" ++ service ti ++ "-RPC"
    serviceName (ComputeApi ti _) = project ti ++ "-" ++ service ti ++ "-ComputeApi"
    serviceName (NovaImage  ti _) = project ti ++ "-" ++ service ti ++ "-NovaImage"
    serviceName (NovaVirt   ti _) = project ti ++ "-" ++ service ti ++ "-NovaVirt"
    serviceName (NeutronApi ti _) = project ti ++ "-" ++ service ti ++ "-NeutronApi"

    getChildren :: Trace -> [Trace]
    getChildren (Root         ts) = ts
    getChildren (Wsgi       _ ts) = ts
    getChildren (DB         _ ts) = ts
    getChildren (RPC        _ ts) = ts
    getChildren (ComputeApi _ ts) = ts
    getChildren (NovaImage  _ ts) = ts
    getChildren (NovaVirt   _ ts) = ts
    getChildren (NeutronApi _ ts) = ts

seqdiagTop :: Trace -> String
seqdiagTop t = "seqdiag {\n" ++ seqdiag t ++ "\n}"

main :: IO ()
main = do
  json <- BS.readFile "tests/rsc/server-create-real.json"
  writeFile "tests/rsc/out.dot" (maybe "nothing" seqdiagTop (decodeTrace json))
