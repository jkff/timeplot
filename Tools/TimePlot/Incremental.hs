{-# LANGUAGE GADTs, BangPatterns #-}
module Tools.TimePlot.Incremental where

import Data.Time
import qualified Data.List as L
import qualified Data.Map as M
import Control.Applicative

data StreamSummary a r where
  Summary :: { insert :: a -> StreamSummary a r, finalize :: r } -> StreamSummary a r

instance Functor (StreamSummary a) where
  fmap f (Summary insert res) = Summary (fmap f . insert) (f res)

instance Applicative (StreamSummary a) where
  pure r = Summary (\_ -> pure r)  r
  (!fs) <*> (!xs) = Summary (\a -> insert fs a <*> insert xs a) (finalize fs $ finalize xs)

runStreamSummary :: StreamSummary a r -> [a] -> r
runStreamSummary !s []     = finalize s
runStreamSummary !s (a:as) = runStreamSummary (insert s a) as

stateful :: s -> (a -> s -> s) -> (s -> r) -> StreamSummary a r
stateful init insert finalize = go init
  where
    go !s = Summary (\a -> go (insert a s)) (finalize s)

filterMap :: (a -> Maybe b) -> StreamSummary b r -> StreamSummary a r
filterMap p s@(Summary insert res) = Summary insert' res
  where 
    insert' a = case p a of { Nothing -> filterMap p s ; Just b -> filterMap p (insert b) }

mapInput :: (a -> b) -> StreamSummary b r -> StreamSummary a r
mapInput f (Summary insert res) = Summary (mapInput f . insert . f) res

collect :: StreamSummary a [a]
collect = stateful [] (:) reverse

byTimeBins :: (Ord t) => [t] -> StreamSummary (t,[a]) r -> StreamSummary (t,a) r
byTimeBins ts s = stateful init' insert' finalize'
  where
    init' = (ts, [], s)
    insert' (t,a) (t1:t2:ts, curBin, !s) 
      | t < t1 = error "Times are not in ascending order"
      | t < t2 = (t1:t2:ts, a:curBin, s)
      | True   = (t2:ts, [a], insert s (t1,reverse curBin))
    finalize' (t1:t2:ts, curBin, s) = finalize (insert s (t1,reverse curBin))


byKey :: (Ord k) => (k -> StreamSummary v r) -> StreamSummary (k,v) (M.Map k r)
byKey initByKey = stateful init' insert' finalize'
  where
    init' = M.empty
    insert' (k,v) m = case M.lookup k m of
      Nothing -> M.insert k (insert (initByKey k) v) m
      Just !s -> M.insert k (insert s v) m
    finalize' = fmap finalize
