{-# LANGUAGE ParallelListComp, ScopedTypeVariables, BangPatterns #-}
module Tools.TimePlot.Plots (
    initGen
) where

import Control.Monad
import qualified Control.Monad.Trans.State.Strict as St
import Control.Arrow
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as S

import Data.Time

import Data.Accessor

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Event

import Data.Colour
import Data.Colour.Names

import Data.Monoid

import Tools.TimePlot.Types
import Tools.TimePlot.Incremental

type PlotGen = String -> UTCTime -> UTCTime -> StreamSummary (UTCTime, InEvent) PlotData

initGen :: ChartKind UTCTime -> PlotGen
initGen (KindACount bs)         = genActivity (\sns n -> n) bs
initGen (KindAPercent bs b)     = genActivity (\sns n -> 100*n/b) bs
initGen (KindAFreq bs)          = genActivity (\sns n -> if n == 0 then 0 else (n / sum (M.elems sns))) bs
initGen (KindFreq bs k)         = genAtoms atoms2freqs bs k
  where  atoms2freqs as m = let n = length as in [fromIntegral (M.findWithDefault 0 a m)/fromIntegral n | a <- as]
initGen (KindHistogram bs k)    = genAtoms atoms2hist bs k
  where  atoms2hist as m = [fromIntegral (M.findWithDefault 0 a m) | a <- as]
initGen KindEvent               = genEventPlot
initGen (KindQuantile bs vs)    = genQuantile bs vs
initGen (KindBinFreq  bs vs)    = genBinFreqs bs vs
initGen (KindBinHist  bs vs)    = genBinHist  bs vs
initGen KindLines               = genLines
initGen (KindDots     alpha)    = genDots alpha
initGen (KindSum bs ss)         = genSum bs ss
initGen (KindCumSum ss)         = genCumSum ss
initGen (KindDuration sk)       = genDuration sk
initGen (KindWithin _ _)        = \name -> error $ "KindDuration should not be plotted: track " ++ show name
initGen KindNone                = \name -> error $ "KindNone should not be plotted: track " ++ show name
initGen KindUnspecified         = \name -> error $ "Kind not specified for track " ++ show name ++ " (have you misspelled -dk or any of -k arguments?)"

plotTrackBars :: [(UTCTime,[Double])] -> [String] -> String -> [Colour Double] -> PlotData
plotTrackBars values titles name colors = PlotBarsData {
        plotName = name,
        barsStyle = BarsStacked,
        barsValues = values,
        barsStyles = [(solidFillStyle c, Nothing) | (i,_) <- [0..]`zip`titles | c <- transparent:map opaque colors],
        barsTitles = titles
    }

plotLines :: String -> [(S.ByteString, [(UTCTime,Double)])] -> PlotData
plotLines name vss = PlotLinesData {
        plotName = name,
        linesData = [vs | (_, vs) <- vss],
        linesStyles = [solidLine 1 color | _ <- vss | color <- map opaque colors],
        linesTitles = [S.unpack subtrack | (subtrack, _) <- vss]
    }

genValues (t,InValue s v) = Just (t,s,v)
genValues _               = Nothing

genEdges (t,InEdge s e) = Just (t,s,e)
genEdges _              = Nothing

valuesDropTrack (t, InValue s v) = Just (t,v)
valuesDropTrack _                = Nothing

atomsDropTrack (t, InAtom s a) = Just (t,a)
atomsDropTrack _               = Nothing

genLines :: PlotGen
genLines name t0 t1 = genFilterMap genValues $ listSummary (data2plot . groupByTrack)
  where
    data2plot vss = PlotLinesData {
        plotName = name,
        linesData = [vs | (_,vs) <- vss],
        linesStyles = [solidLine 1 color | _ <- vss | color <- map opaque colors],
        linesTitles = [S.unpack subtrack | (subtrack, _) <- vss]
      }

genDots :: Double -> PlotGen
genDots alpha name t0 t1 = genFilterMap genValues $ listSummary (data2plot . groupByTrack)
  where
    data2plot vss = PlotDotsData {
        plotName = name,
        dotsData = [vs | (_,vs) <- vss],
        dotsTitles = [S.unpack subtrack | (subtrack, _) <- vss],
        dotsColors = if alpha == 1 then map opaque colors else map (`withOpacity` alpha) colors
      }

summaryByFixedTimeBins t0 binSize = summaryByTimeBins (iterate (add binSize) t0)

lag :: [a] -> [(a,a)]
lag xs = xs `zip` tail xs

genByBins :: ([Double] -> [Double] -> [Double]) -> NominalDiffTime -> [Double] -> PlotGen
genByBins f binSize vs name t0 t1 = genFilterMap valuesDropTrack $ 
    summaryByFixedTimeBins t0 binSize $
    mapInputSummary (\(t,xs) -> (t, 0:f vs xs)) $
    listSummary (\tfs -> plotTrackBars tfs (binTitles vs) name colors)
  where
    binTitles vs = [low]++[show v1++".."++show v2 | (v1,v2) <- lag vs]++[high]
      where
        low = "<"++show (head vs)
        high = ">"++show (last vs)

genBinHist :: NominalDiffTime -> [Double] -> PlotGen
genBinHist = genByBins values2binHist

genBinFreqs :: NominalDiffTime -> [Double] -> PlotGen
genBinFreqs = genByBins values2binFreqs

values2binFreqs :: (Ord a) => [a] -> [a] -> [Double]
values2binFreqs bins xs = map toFreq $ values2binHist bins xs
  where
    n = length xs
    toFreq = if n==0 then const 0 else (\k -> k/fromIntegral n)

values2binHist bins xs = values2binHist' bins $ sort xs
  where
    values2binHist' []     xs = [fromIntegral (length xs)]
    values2binHist' (a:as) xs = fromIntegral (length xs0) : values2binHist' as xs'
      where (xs0,xs') = span (<a) xs

genQuantile :: NominalDiffTime -> [Double] -> PlotGen
genQuantile binSize qs name t0 t1 = genFilterMap valuesDropTrack $
    summaryByFixedTimeBins t0 binSize $
    mapInputSummary (\(t,xs) -> (t,diffs (getQuantiles qs xs))) $
    listSummary (\tqs -> plotTrackBars tqs quantileTitles name colors)
  where
    quantileTitles = [""]++[show p1++".."++show p2++"%" | (p1,p2) <- lag percents ]
    percents = map (floor . (*100.0)) $ [0.0] ++ qs ++ [1.0]
    diffs xs = zipWith (-) xs (0:xs)

getQuantiles :: (Ord a) => [Double] -> [a] -> [a]
getQuantiles qs = \xs -> quantiles' (sort xs)
  where
    qs' = sort qs
    quantiles' [] = []
    quantiles' xs = index (0:ns++[n-1]) 0 xs
      where
        n  = length xs
        ns = map (floor . (*(fromIntegral n-1))) qs'

        index _         _ []     = []
        index []        _ _      = []
        index [i]       j (x:xs)
          | i<j  = []
          | i==j  = [x]
          | True = index [i] (j+1) xs
        index (i:i':is) j (x:xs)
          | i<j  = index (i':is)   j     (x:xs)
          | i>j  = index (i:i':is) (j+1) xs
          | i==i' = x:index (i':is) j     (x:xs)
          | True = x:index (i':is) (j+1) xs

genAtoms :: ([S.ByteString] -> M.Map S.ByteString Int -> [Double]) ->
           NominalDiffTime -> PlotBarsStyle -> PlotGen
genAtoms f binSize k name t0 t1 = genFilterMap atomsDropTrack $
    mapOutputSummary (uncurry h) (teeSummary uniqueAtoms fInBins)
  where 
    fInBins :: StreamSummary (UTCTime, S.ByteString) [(UTCTime, M.Map S.ByteString Int)]
    fInBins = summaryByFixedTimeBins t0 binSize $
              mapInputSummary (\(t,as) -> (t, counts as)) $
              listSummary id
    counts  = foldl' insert M.empty
      where
        insert m a = case M.lookup a m of
          Nothing -> M.insert a 1     m
          Just !n -> M.insert a (n+1) m

    uniqueAtoms :: StreamSummary (UTCTime,S.ByteString) [S.ByteString]
    uniqueAtoms = mapInputSummary (\(t,a) -> a) $ statefulSummary M.empty (\a -> M.insert a ()) M.keys

    h :: [S.ByteString] -> [(UTCTime, M.Map S.ByteString Int)] -> PlotData
    h as tfs = plotTrackBars (map (\(t,counts) -> (t,f as counts)) tfs) (map show as) name colors

uniqueSubtracks :: StreamSummary (UTCTime,S.ByteString,a) [S.ByteString]
uniqueSubtracks = mapInputSummary (\(t,s,a) -> s) $ statefulSummary M.empty (\a -> M.insert a ()) M.keys

genSum :: NominalDiffTime -> SumSubtrackStyle -> PlotGen
genSum binSize ss name t0 t1 = genFilterMap genValues $
    mapOutputSummary (uncurry h) (teeSummary uniqueSubtracks sumsInBins)
  where 
    sumsInBins :: StreamSummary (UTCTime,S.ByteString,Double) [(UTCTime, M.Map S.ByteString Double)]
    sumsInBins = mapInputSummary (\(t,s,v) -> (t,(s,v))) $
                 summaryByFixedTimeBins t0 binSize $
                 mapInputSummary (\(t,tvs) -> (t, fromListWith' (+) tvs)) $
                 listSummary id

    h :: [S.ByteString] -> [(UTCTime, M.Map S.ByteString Double)] -> PlotData
    h tracks binSums = plotLines name rows
      where
        rowsT' = case ss of
          SumOverlayed -> map (\(t,ss) -> (t, M.toList ss)) binSums
          SumStacked   -> map (\(t,ss) -> (t, stack ss))    binSums

        stack :: M.Map S.ByteString Double -> [(S.ByteString, Double)]
        stack ss = zip tracks (scanl1 (+) (map (\x -> M.findWithDefault 0 x ss) tracks))
        
        rows :: [(S.ByteString, [(UTCTime, Double)])]
        rows = M.toList $ fmap sort $ M.fromListWith (++) $
          [(track, [(t,sum)]) | (t, m) <- rowsT', (track, sum) <- m]

genCumSum :: SumSubtrackStyle -> PlotGen
genCumSum ss name t0 t1 = genFilterMap genValues $ listSummary (plotLines name . data2plot ss)
  where
    data2plot :: SumSubtrackStyle -> [(UTCTime, S.ByteString, Double)] -> [(S.ByteString, [(UTCTime, Double)])]
    data2plot SumOverlayed es = [(s, scanl (\(t1,s) (t2,v) -> (t2,s+v)) (t0, 0) vs) | (s, vs) <- groupByTrack es]
    data2plot SumStacked   es = rows
      where
        allTracks = Set.toList $ Set.fromList [track | (t, track, v) <- es]

        rows :: [(S.ByteString, [(UTCTime, Double)])]
        rows = groupByTrack [(t, track, v) | (t, tvs) <- rowsT, (track,v) <- tvs]

        rowsT :: [(UTCTime, [(S.ByteString, Double)])]
        rowsT = (t0, zip allTracks (repeat 0)) : St.evalState (mapM addDataPoint es) M.empty
        
        addDataPoint (t, track, v) = do
          St.modify (M.insertWith' (+) track v)
          st <- St.get
          let trackSums = map (\x -> M.findWithDefault 0 x st) allTracks
          return (t, allTracks `zip` (scanl1 (+) trackSums))
 
genActivity :: (M.Map S.ByteString Double -> Double -> Double) -> NominalDiffTime -> PlotGen
genActivity f bs name t0 t1 = genFilterMap genEdges $
    mapOutputSummary (uncurry h) (teeSummary uniqueSubtracks binAreas)
  where 
    binAreas :: StreamSummary (UTCTime,S.ByteString,Edge) [(UTCTime, M.Map S.ByteString Double)]
    binAreas = mapOutputSummary (map (\((t1,t2),m) -> (t1,m))) $ edges2binsSummary bs t0 t1

    h tracks binAreas = (plotTrackBars barsData ("":map S.unpack tracks) name colors) { barsStyle = BarsStacked }
      where
        barsData = [(t, 0:map (f m . flip (M.findWithDefault 0) m) tracks) | (t,m) <- binAreas]

edges2binsSummary :: (Ord t,HasDelta t,Show t) => Delta t -> t -> t -> StreamSummary (t,S.ByteString,Edge) [((t,t), M.Map S.ByteString Double)]
edges2binsSummary binSize tMin tMax = statefulSummary (M.empty, iterate (add binSize) tMin, []) step flush
  where
    modState s t (!m, ts,r) f = {-# SCC "modState" #-} (m', ts, r)
      where
        m' = M.insertWith' (\new !old -> f old) s (f (0,t,0,0)) m

    flushBin st@(m,t1:t2:ts,!r) = {-# SCC "flushBin" #-} (m', t2:ts, r')
      where
        states = M.toList m
        binSizeSec = toSeconds (t2 `sub` t1) t1
        !r' = ((t1,t2), M.fromList [(s, (fromIntegral npulse + area + toSeconds (t2 `sub` start) t2*nopen)/binSizeSec) 
                                   |(s,(area,start,nopen,npulse)) <- states]):r
        !m' = fmap (\(_,_,nopen,_) -> (0,t2,nopen,0)) m

    step ev@(t, s, e) st@(m, t1:t2:ts, r)
      | t < t1  = error "Times are not in ascending order"
      | t >= t2 = step ev (flushBin st)
      | True    = step'' ev st

    step'' ev@(t,s,e) st@(m, t1:t2:ts, r) = if (t < t1 || t >= t2) then error "Outside bin" else step' ev st
    step' (t, s, SetTo _) st = st
    step' (t, s, Pulse _) st = modState s t st $ \(!area, !start, !nopen, !npulse) -> (area,                                   t, nopen,   npulse+1)
    step' (t, s, Rise)    st = modState s t st $ \(!area, !start, !nopen, !npulse) -> (area+toSeconds (t `sub` start) t*nopen, t, nopen+1, npulse)
    step' (t, s, Fall)    st = modState s t st $ \(!area, !start, !nopen, !npulse) -> (area+toSeconds (t `sub` start) t*nopen, t, nopen-1, npulse)
    flush st@(m, t1:t2:ts, r) 
      | t2 <= tMax = flush (flushBin st)
      | True       = reverse r

edges2eventsSummary' :: forall t a . (Ord t) => t -> t -> StreamSummary (S.ByteString, Event t Status) a -> StreamSummary (t,S.ByteString,Edge) a
edges2eventsSummary' t0 t1 s = statefulSummary (M.empty,s) step flush
  where
    getTrackStates   (t, _) = t
    getSummary       (_, s) = s
    tellSummary    e (t,!s) = (t,genInsert s e)

    getTrack  s   (!ts,sum) = M.findWithDefault (t0, 0, emptyStatus) s ts
    putTrack  s t (!ts,sum) = (M.insert s t ts, sum)
    killTrack s   (!ts,sum) = (M.delete s   ts, sum)
    trackCase s whenZero withNonzero st 
        | numActive == 0 = whenZero
        | True           = withNonzero t0 numActive status
      where (t0, numActive, status) = getTrack s st

    emptyStatus = Status "" ""

    step (t,s,Pulse st) state = tellSummary (s, PulseEvent t st) state
    step (t,s,SetTo st) state = trackCase s 
                                      (putTrack s (t, 1, st) state)
                                      (\t0 !n st0 -> putTrack s (t,n,st) $ tellSummary (s, LongEvent (t0,True) (t,True) st0) state)
                                      state
    step (t,s,Rise)     state = trackCase s 
                                      (putTrack s (t, 1, emptyStatus) state) 
                                      (\t0 !n st  -> putTrack s (t, n+1, st) state)
                                      state
    step (t,s,Fall)     state
        | numActive == 1 = killTrack s $ tellSummary (s, LongEvent (t0,True) (t,True) st) state
        | True           = putTrack s (t0, max 0 (numActive-1), st) state
      where
        (t0, numActive, st) = getTrack s state

    flush (ts,sum) = genResult $ foldl' addEvent sum $ M.toList ts
      where
        addEvent sum (s,(t0,_,st)) = genInsert sum (s, LongEvent (t0,True) (t1,False) st)

edges2eventsSummary :: (Ord t) => t -> t -> StreamSummary (t,S.ByteString,Edge) [(S.ByteString,Event t Status)]
edges2eventsSummary t0 t1 = edges2eventsSummary' t0 t1 (listSummary id)

edges2durationsSummary :: forall t a . (Ord t, HasDelta t) => t -> t -> String -> StreamSummary (t,InEvent) a -> StreamSummary (t,S.ByteString,Edge) a
edges2durationsSummary t0 t1 commonTrack = edges2eventsSummary' t0 t1 . genFilterMap genDurations
  where
    genDurations (track, LongEvent (t1,True) (t2,True) _) = Just (t2, InValue commonTrackBS $ toSeconds (t2 `sub` t1) (undefined :: t))
    genDurations _                                        = Nothing
    commonTrackBS = S.pack commonTrack

genEventPlot :: PlotGen
genEventPlot name t0 t1 = genFilterMap genEdges $ 
                          mapOutputSummary (\evs -> PlotEventData { plotName = name, eventData = map snd evs }) $ 
                          edges2eventsSummary t0 t1
-- TODO Multiple tracks

genDuration :: ChartKind UTCTime -> PlotGen
genDuration sk name t0 t1 = genFilterMap genEdges $ edges2durationsSummary t0 t1 name (initGen sk name t0 t1)

-- UTILITIES

fromListWith' f kvs = foldl' insert M.empty kvs
  where
    insert m (k,v) = case M.lookup k m of
      Nothing  -> M.insert k v        m
      Just !v' -> M.insert k (f v' v) m

colors = cycle [green,blue,red,brown,yellow,orange,grey,purple,violet,lightblue]

groupByTrack xs = M.toList $ sort `fmap` M.fromListWith (++) [(s, [(t,v)]) | (t,s,v) <- xs]

