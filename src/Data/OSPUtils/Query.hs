module Data.OSPUtils.Query where

import Prelude as P

import Data.OSPUtils.Trace

map :: (Trace -> b) -> Trace -> b
map f t = undefined

foldl :: (b -> Trace -> b) -> b -> Trace -> b
foldl g z t = undefined


filter :: (Trace -> Bool) -> Trace -> Trace
filter p t | p t       = filter' t (children t)
           | otherwise = Root []
  where
    filter' :: Trace -> [Trace] -> Trace
    filter' r = setChildren r .
                P.map (\t' -> filter' t' (children t')) . P.filter p

-- agg :: (Trace -> Trace -> Bool) -> Trace -> Trace
-- agg p t = agg' t (children t)
--   where
--     agg' :: Trace -> [ Trace ] -> Trace
--     agg' r ts = setChildren r (foldr (\t' -> ts' -> ) [] ts)
