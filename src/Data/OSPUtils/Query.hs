module Data.OSPUtils.Query where

import Prelude as P
import Data.Tree
import Data.Tree.Zipper

import Data.OSPUtils.Trace

filter :: (TraceType -> Bool) -> Trace -> Trace
filter p t | p (rootLabel t) = filter' t
           | otherwise       = Node Root []
  where
    filter' :: Trace -> Trace
    filter' r = let filterChildren = P.filter (p . rootLabel)
                    filterGrandchildren = map filter'
                in r { subForest = filterGrandchildren (
                                     filterChildren (subForest r)) }

aggregate :: (TraceType -> TraceType -> Bool) -> Trace -> Trace
aggregate ag t = undefined
