module Data.OSPUtils.Seqdiag where

import Prelude

import Data.OSPUtils.Trace
import Data.OSPUtils.Query as TQ

import qualified Data.ByteString.Lazy as BS (readFile, writeFile)


seqdiag :: Trace -> String
seqdiag t = concat $ seqdiag' serviceName t (children t)
  where
    seqdiag' :: (Trace -> String) -> Trace -> [Trace] -> [String]
    seqdiag' f t = map (\t' -> f t ++ " => " ++ f t' ++
                         if null (children t') then ";\n"
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

seqdiagTop :: Trace -> String
seqdiagTop t = "seqdiag {\n" ++ seqdiag t ++ "\n}"

main :: IO ()
main = do
  json <- BS.readFile "tests/rsc/server-create-real.json"
  writeFile "tests/rsc/out.dot" (maybe "nothing" (seqdiagTop . TQ.filter predicate) (decodeTrace json))
  where
    predicate :: Trace -> Bool
    predicate (DB _ _) = False
    predicate _        = True
