{-# LANGUAGE GADTs, BangPatterns #-}
module Tools.TimePlot.Incremental where

import Data.Time
import Data.List
import qualified Data.Map as M

data StreamSummary a r where
  Summary :: { genInsert :: a -> StreamSummary a r, genResult :: r } -> StreamSummary a r

runStreamSummary :: StreamSummary a r -> [a] -> r
runStreamSummary (Summary insert res) []     = res
runStreamSummary (Summary insert res) (a:as) = runStreamSummary (insert a) as

statefulSummary :: s -> (a -> s -> s) -> (s -> r) -> StreamSummary a r
statefulSummary init insert finalize = go init
  where
    go !s = Summary (\a -> go (insert a s)) (finalize s)

genFilterMap :: (a -> Maybe b) -> StreamSummary b r -> StreamSummary a r
genFilterMap p s@(Summary insert res) = Summary insert' res
  where 
    insert' a = case p a of { Nothing -> genFilterMap p s ; Just b -> genFilterMap p (insert b) }

constSummary :: b -> StreamSummary a b
constSummary b = Summary (\a -> constSummary b) b

mapInputSummary :: (a -> b) -> StreamSummary b r -> StreamSummary a r
mapInputSummary f (Summary insert res) = Summary (mapInputSummary f . insert . f) res

mapOutputSummary :: (r1 -> r2) -> StreamSummary a r1 -> StreamSummary a r2
mapOutputSummary f (Summary insert res) = Summary (mapOutputSummary f . insert) (f res)

listSummary :: ([a] -> b) -> StreamSummary a b
listSummary f = statefulSummary [] (:) (f . reverse)

summaryByTimeBins :: (Ord t) => [t] -> StreamSummary (t,[a]) r -> StreamSummary (t,a) r
summaryByTimeBins ts s = statefulSummary init' insert' finalize'
  where
    init' = (ts, [], s)
    insert' (t,a) (t1:t2:ts, curBin, !s) 
      | t < t1 = error "Times are not in ascending order"
      | t < t2 = (t1:t2:ts, a:curBin, s)
      | True   = (t2:ts, [a], genInsert s (t1,reverse curBin))
    finalize' (t1:t2:ts, curBin, s) = genResult (genInsert s (t1,reverse curBin))

summaryByKey :: (Ord k) => (k -> StreamSummary v r) -> StreamSummary (k,v) (M.Map k r)
summaryByKey initByKey = statefulSummary init insert finalize
  where
    init = M.empty
    insert (k,v) m = case M.lookup k m of
      Nothing -> M.insert k (initByKey k) m
      Just !s -> M.insert k (genInsert s v) m
    finalize = fmap genResult

sumSummary = statefulSummary 0 (\a b -> a `seq` b `seq` (a+b)) id
