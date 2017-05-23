module Data.OSPUtils.Seqdiag where

import Prelude

import Data.OSPUtils.Trace
import Data.OSPUtils.Query

import Data.Maybe
import qualified System.Process as S (system)
import qualified Data.List as L
import qualified Data.List.Extra as LE (replace)
import qualified Data.Tree as T
import qualified Data.ByteString.Lazy as BS (readFile, writeFile)


seqdiag :: Trace -> String
seqdiag t = concat $ seqdiag' serviceName t (T.subForest t)
  where
    seqdiag' :: (TraceType -> String) -> Trace -> [Trace] -> [String]
    seqdiag' f t = map (\t' -> f (T.rootLabel t) ++ " => "++ f (T.rootLabel t') ++ label (T.rootLabel t') ++
                         if null (T.subForest t') then ";\n"
                         else " {\n" ++ seqdiag t' ++ "}\n")

    serviceName :: TraceType -> String
    serviceName Root            = "Client"
    serviceName (Wsgi       ti) = project ti ++ "-" ++ service ti ++ "-" ++ host ti
    serviceName (DB         ti) = project ti ++ "-" ++ service ti ++ "-" ++ host ti
    serviceName (RPC        ti) = project ti ++ "-" ++ service ti ++ "-" ++ host ti
    serviceName (ComputeApi ti) = project ti ++ "-" ++ service ti ++ "-" ++ host ti
    serviceName (NovaImage  ti) = project ti ++ "-" ++ service ti ++ "-" ++ host ti
    serviceName (NovaVirt   ti) = project ti ++ "-" ++ service ti ++ "-" ++ host ti
    serviceName (NeutronApi ti) = project ti ++ "-" ++ service ti ++ "-" ++ host ti

    label :: TraceType -> String
    label Root                  = "bla"
    label (Wsgi             ti) = "[label=\""
                                    ++ show (method (req ti))
                                    ++ " "
                                    ++ path(req ti)
                                    ++ "\", color=blue]"
    label (DB               ti) = "[label=\"" ++ stmt (req ti)  ++ "\"]"
    label (RPC              ti) = "[label=\"" ++ function (req ti)  ++ "\", color=green]"
    label (ComputeApi       ti) = "[label=\"" ++ function (req ti)  ++ "\"]"
    label (NovaImage        ti) = "[label=\"" ++ function (req ti)  ++ "\"]"
    label (NovaVirt         ti) = "[label=\"" ++ function (req ti)  ++ "\"]"
    label (NeutronApi       ti) = "[label=\"" ++ function (req ti)  ++ "\"]"


seqdiagTop :: Trace -> String
seqdiagTop t = "seqdiag {\n" ++ seqdiag t ++ "\n}"

fileToTrace :: String -> IO Trace
fileToTrace fp = do
  json <- BS.readFile fp
  pure $ fromMaybe (T.Node Root []) (decodeTrace json)


tracesFilePath :: [String]
tracesFilePath =
  [ "tests/rsc/empty-root.json"
  , "tests/rsc/wsgi.json"
  , "tests/rsc/neutron-db.json"
  , "tests/rsc/trace-boot-and-associate-floating-ip.yaml.json"
  , "tests/rsc/trace-boot-and-delete.yaml.json"
  , "tests/rsc/trace-boot-server-and-add-secgroup.yaml.json"
  , "tests/rsc/trace-create-and-delete-image.yaml.json"
  , "tests/rsc/trace-create-and-delete-networks.yaml.json"
  , "tests/rsc/trace-create-and-delete-ports.yaml.json"
  , "tests/rsc/trace-create-and-delete-routers.yaml.json"
  , "tests/rsc/trace-create-and-delete-security-groups.yaml.json"
  , "tests/rsc/trace-create-and-delete-subnets.yaml.json"
  , "tests/rsc/trace-pause-and-unpause.yaml.json"
  ]

main :: IO ()
main = do
  let q = Data.OSPUtils.Query.filter p'
  ts <- mapM fileToTrace tracesFilePath
  let tsNamed = zip tracesFilePath ts
  -- Print the number of call into each scenario
  putStrLn $ L.intercalate "\n" $ map (\(n,t) -> n ++ " size: " ++ show (L.length t)) tsNamed
  -- Produces txt/dot/seqdiag files
  putStrLn "Produces text files ..."
  _ <- mapM (\(n,t) -> writeFile (LE.replace ".json" ".txt" n) (T.drawTree $ show <$> t)) tsNamed
  putStrLn "Produces dot files ..."
  _ <- mapM (\(n,t) -> writeFile (LE.replace ".json" ".dot" n) (seqdiagTop $ q t)) tsNamed
  putStrLn "Produces seqdiag files ..."
  _ <- mapM (\(n,_) -> S.system ("seqdiag -Tsvg " ++ LE.replace ".json" ".dot" n)) tsNamed
  pure ()
  where
    p :: TraceType -> TraceType -> Bool
    -- p (Wsgi _) (Wsgi _) = True
    p (DB   _) (DB   _) = True
    p _        _        = False

    p' :: TraceType -> Bool
    -- p' (Wsgi _) = True
    p' (RPC _) = True
    p' _ = False
