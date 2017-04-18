module Data.OSPUtils.Seqdiag where

import Prelude

import Data.Tree
import Data.OSPUtils.Trace

import qualified Data.ByteString.Lazy as BS (readFile)


seqdiag :: Trace -> String
seqdiag t = concat $ seqdiag' serviceName t (subForest t)
  where
    seqdiag' :: (TraceType -> String) -> Trace -> [Trace] -> [String]
    seqdiag' f t = map (\t' -> f (rootLabel t) ++ " => " ++ f (rootLabel t') ++
                         if null (subForest t') then ";\n"
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

main :: IO ()
main = do
  json <- BS.readFile "tests/rsc/server-create-real.json"
  writeFile "tests/rsc/out.dot" (maybe "nothing" seqdiagTop (decodeTrace json))
  where
    predicate :: Trace -> Bool
    predicate (Node (DB _) _) = False
    predicate _               = True
