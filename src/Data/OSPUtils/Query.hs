module Data.OSPUtils.Query where

import Prelude hiding (filter, scanl1)
import qualified Data.List as L
import qualified Data.Tree as T

import Data.OSPUtils.Trace

-- | Filters trace by keeping those that satisfy the predicate.
filter :: (TraceType -> Bool) -> Trace -> Trace
filter p t = let filteredChildren = doChildren t
             in t { T.subForest = L.map (filter p) filteredChildren }
  where
    doChildren :: Trace -> [Trace]
    doChildren = L.filter (p . T.rootLabel) . T.subForest

-- | Folds traces of the same level by first grouping them using
-- grouping predicate based on TraceType and then reducing each group
-- of Traces into one Trace using a concatenation function.
foldBy :: ([Trace] -> Trace) -> (TraceType -> TraceType -> Bool) -> Trace -> Trace
foldBy agg grp t =
  let aggregatedChildren = doChildren
  in t { T.subForest = L.map (foldBy agg grp) aggregatedChildren }

  where
    doChildren :: [Trace]
    doChildren = L.map agg $ L.groupBy (lift grp) (T.subForest t)

    lift :: (TraceType -> TraceType -> Bool) -> Trace -> Trace -> Bool
    lift f (T.Node tty _) (T.Node tty' _) = f tty tty'

fold :: (TraceType -> TraceType -> Bool) -> Trace -> Trace
fold = foldBy concatTrace
  where
    -- Concatenate a group a trace into one trace. It keeps the
    -- TraceType of the first trace and sets the stop time with the
    -- older one. Children of all group are concatenated in the final
    -- trace.
    concatTrace :: [Trace] -> Trace
    concatTrace ts =
      let (ttyFirst, ttyLast) = headAndLast $ L.map T.rootLabel ts
          newChildren         = L.concatMap T.subForest ts
          newTTy              = updateTTy ttyFirst ttyLast
      in T.Node newTTy newChildren

    headAndLast xs = (head xs, last xs)

    updateTTy :: TraceType -> TraceType -> TraceType
    updateTTy (Wsgi       ti) tty = Wsgi       ti { stop = getStopTime tty }
    updateTTy (DB         ti) tty = DB         ti { stop = getStopTime tty }
    updateTTy (RPC        ti) tty = RPC        ti { stop = getStopTime tty }
    updateTTy (ComputeApi ti) tty = ComputeApi ti { stop = getStopTime tty }
    updateTTy (NovaImage  ti) tty = NovaImage  ti { stop = getStopTime tty }
    updateTTy (NovaVirt   ti) tty = NovaVirt   ti { stop = getStopTime tty }
    updateTTy (NeutronApi ti) tty = NeutronApi ti { stop = getStopTime tty }
    updateTTy Root            _   = Root

    getStopTime :: TraceType -> Maybe String
    getStopTime (Wsgi       ti) = stop ti
    getStopTime (DB         ti) = stop ti
    getStopTime (RPC        ti) = stop ti
    getStopTime (ComputeApi ti) = stop ti
    getStopTime (NovaImage  ti) = stop ti
    getStopTime (NovaVirt   ti) = stop ti
    getStopTime (NeutronApi ti) = stop ti
    getStopTime Root            = Nothing

scanl1 :: (TraceType -> TraceType -> TraceType) -> Trace -> Trace
scanl1 f t = let scannedChildren = doChildren t
             in t { T.subForest = L.map (scanl1 f) scannedChildren }
  where
    doChildren :: Trace -> [Trace]
    doChildren = L.scanl1 (lift f) . T.subForest

    lift :: (TraceType -> TraceType -> TraceType) -> Trace -> Trace -> Trace
    lift f (T.Node tty c) (T.Node tty' c') = T.Node (f tty tty') (c ++ c')
