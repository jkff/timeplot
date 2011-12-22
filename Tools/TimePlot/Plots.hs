{-# LANGUAGE ParallelListComp, ScopedTypeVariables, BangPatterns #-}
module Tools.TimePlot.Plots (
    plotTrack,
    plotTrackACount,
    plotTrackAFreq,
    plotTrackAPercent,
    plotTrackFreq,
    plotTrackHist,
    plotTrackEvent,
    plotTrackQuantile,
    plotTrackBinFreqs,
    plotTrackBinHist,
    plotTrackLines,
    plotTrackDots,
    plotTrackSum,
    plotTrackCumSum,

    initGen,
    genLines,
    genDots
) where

import Control.Monad
import qualified Control.Monad.Trans.State.Strict as St
import qualified Control.Monad.Trans.RWS.Strict as RWS
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

import Tools.TimePlot.Types
import Tools.TimePlot.Incremental

type PlotGen = StreamSummary (LocalTime, InEvent) PlotData

plotTrack :: String -> ChartKind LocalTime -> [(LocalTime, InEvent)] -> LocalTime -> LocalTime -> PlotData
plotTrack name k es minInTime maxInTime = case k of
  KindACount    bs    -> plotTrackACount    name es minInTime maxInTime bs
  KindAPercent  bs b  -> plotTrackAPercent  name es minInTime maxInTime bs b
  KindAFreq     bs    -> plotTrackAFreq     name es minInTime maxInTime bs
  KindFreq      bs k  -> plotTrackFreq      name es minInTime bs k
  KindHistogram bs k  -> plotTrackHist      name es minInTime bs k
  KindEvent           -> plotTrackEvent     name es minInTime maxInTime 
  KindQuantile  bs qs -> plotTrackQuantile  name es minInTime qs bs
  KindBinFreq   bs vs -> plotTrackBinFreqs  name es minInTime vs bs
  KindBinHist   bs vs -> plotTrackBinHist   name es minInTime vs bs
  KindLines           -> plotTrackLines     name es
  KindDots      alpha -> plotTrackDots      name es alpha
  KindSum       bs ss -> plotTrackSum       name es minInTime bs ss
  KindCumSum    ss    -> plotTrackCumSum    name es minInTime ss
  KindDuration  sk    -> plotTrack          name sk (edges2durations (edges es) minInTime maxInTime name) minInTime maxInTime
  KindWithin    _ _   -> error $ "KindDuration should not be plotted: track " ++ show name
  KindNone            -> error $ "KindNone should not be plotted: track " ++ show name
  KindUnspecified     -> error $ "Kind not specified for track " ++ show name ++ " (have you misspelled -dk or any of -k arguments?)"


plotTrackActivity :: String -> [(LocalTime,InEvent)] -> LocalTime -> LocalTime -> 
                     NominalDiffTime -> ([(S.ByteString, Double)] -> Double -> Double) -> PlotData
plotTrackActivity name es minInTime maxInTime bs transform = PlotBarsData {
        plotName = name,
        barsStyle = BarsStacked,
        barsValues = barsData,
        barsStyles = itemStyles,
        barsTitles = map show subTracks
    }
  where itemStyles = [(solidFillStyle (opaque c), Nothing) | c <- colors]
        bins = edges2bins bs minInTime maxInTime (edges es)
        subTracks = Set.toList $ Set.fromList [s | (_,sns) <- bins, (s,n) <- sns]
        barsData = [(t, map (transform sns . fromMaybe 0 . (`lookup` sns)) subTracks) 
                   | ((t,_),sns) <- bins, (s,n) <- sns]

plotTrackACount :: String -> [(LocalTime,InEvent)] -> LocalTime -> LocalTime -> NominalDiffTime -> PlotData
plotTrackACount name es minInTime maxInTime bs = plotTrackActivity name es minInTime maxInTime bs (\_ -> id)

plotTrackAFreq :: String -> [(LocalTime,InEvent)] -> LocalTime -> LocalTime -> NominalDiffTime -> PlotData
plotTrackAFreq name es minInTime maxInTime bs = plotTrackActivity name es minInTime maxInTime bs $ \sns -> 
    let total = (\x -> if x==0 then 1 else x) $ sum [n | (s,n) <- sns] in (/total)

plotTrackAPercent :: String -> [(LocalTime,InEvent)] -> LocalTime -> LocalTime -> NominalDiffTime -> Double -> PlotData
plotTrackAPercent name es minInTime maxInTime bs b = plotTrackActivity name es minInTime maxInTime bs (\_ x -> 100*x/b)

plotTrackFreq  :: String -> [(LocalTime,InEvent)] -> LocalTime -> NominalDiffTime -> PlotBarsStyle -> PlotData
plotTrackFreq  = plotTrackAtoms atoms2freqs

plotTrackHist  :: String -> [(LocalTime,InEvent)] -> LocalTime -> NominalDiffTime -> PlotBarsStyle -> PlotData
plotTrackHist  = plotTrackAtoms atoms2hist

atoms2hist :: (Ord a) => [a] -> M.Map a Int -> [Double]
atoms2hist as m = [fromIntegral (M.findWithDefault 0 a m) | a <- as]

atoms2freqs :: (Ord a) => [a] -> M.Map a Int -> [Double]
atoms2freqs as m = [fromIntegral (M.findWithDefault 0 a m)/fromIntegral n | a <- as]
  where
    n = length as

-- TODO Multiple tracks
plotTrackEvent :: String -> [(LocalTime,InEvent)] -> LocalTime -> LocalTime -> PlotData
plotTrackEvent name es minInTime maxInTime = PlotEventData { 
        plotName = name,
        eventData = dropTrack (edges2events (edges es) minInTime maxInTime)
    }
  where dropTrack = map snd
        toFillStyle s = solidFillStyle . opaque $ fromMaybe lightgray (readColourName (statusColor s))
        toLabel     s = statusLabel s

plotTrackQuantile  name es t0 vs bs = runStreamSummary (genQuantile bs vs name t0)        es
plotTrackBinFreqs  name es t0 vs bs = runStreamSummary (genBinFreqs bs vs name t0)        es
plotTrackBinHist   name es t0 vs bs = runStreamSummary (genBinHist  bs vs name t0)        es
plotTrackLines     name es          = runStreamSummary (genLines          name undefined) es
plotTrackDots      name es alpha    = runStreamSummary (genDots     alpha name undefined) es
plotTrackAtoms   f name es t0 bs k  = runStreamSummary (genAtoms f  bs k  name t0)        es
plotTrackSum       name es t0 bs ss = runStreamSummary (genSum      bs ss name t0)        es

plotTrackBars :: [(LocalTime,[Double])] -> [String] -> String -> [Colour Double] -> PlotData
plotTrackBars values titles name colors = PlotBarsData {
        plotName = name,
        barsStyle = BarsStacked,
        barsValues = values,
        barsStyles = [(solidFillStyle c, Nothing) | (i,_) <- [0..]`zip`titles | c <- transparent:map opaque colors],
        barsTitles = titles
    }


plotTrackCumSum :: String -> [(LocalTime,InEvent)] -> LocalTime -> SumSubtrackStyle -> PlotData
plotTrackCumSum name es minInTime SumOverlayed = plotLines name rows
  where rows = [(track, scanl (\(t1,s) (t2,v) -> (t2,s+v)) (minInTime, 0) vs) | (track, vs) <- groupByTrack (values es)]
plotTrackCumSum name es minInTime SumStacked = plotLines name rows
  where vals = values es
        allTracks = Set.toList $ Set.fromList [track | (t, track, v) <- vals]

        rows :: [(S.ByteString, [(LocalTime, Double)])]
        rows = groupByTrack [(t, track, v) | (t, tvs) <- rowsT, (track,v) <- tvs]

        rowsT :: [(LocalTime, [(S.ByteString, Double)])]
        rowsT = (minInTime, zip allTracks (repeat 0)) : St.evalState (mapM addDataPoint vals) M.empty
        
        addDataPoint (t, track, v) = do
          St.modify (M.insertWith (+) track v)
          st <- St.get
          let trackSums = map (\x -> M.findWithDefault 0 x st) allTracks
          return (t, allTracks `zip` (scanl1 (+) trackSums))

edges  :: [(LocalTime,InEvent)] -> [(LocalTime,S.ByteString,Edge)]
values :: [(LocalTime,InEvent)] -> [(LocalTime,S.ByteString,Double)]
edges  es = [(t,s,e) | (t,InEdge  s e) <- es]
values es = [(t,s,v) | (t,InValue s v) <- es]

lag :: [a] -> [(a,a)]
lag xs = xs `zip` tail xs

colors = cycle [green,blue,red,brown,yellow,orange,grey,purple,violet,lightblue]

binTitles vs = [low]++[show v1++".."++show v2 | (v1,v2) <- lag vs]++[high]
  where
    low = "<"++show (head vs)
    high = ">"++show (last vs)

diffs xs = zipWith (-) xs (0:xs)

groupByTrack xs = M.toList $ sort `fmap` M.fromListWith (++) [(s, [(t,v)]) | (t,s,v) <- xs]

plotLines :: String -> [(S.ByteString, [(LocalTime,Double)])] -> PlotData
plotLines name vss = PlotLinesData {
        plotName = name,
        linesData = [vs | (_, vs) <- vss],
        linesStyles = [solidLine 1 color | _ <- vss | color <- map opaque colors],
        linesTitles = [S.unpack subtrack | (subtrack, _) <- vss]
    }

edges2bins :: (Ord t,HasDelta t,Show t) => Delta t -> t -> t -> [(t,S.ByteString,Edge)] -> [((t,t), [(S.ByteString,Double)])]
edges2bins binSize minTime maxTime es = snd $ RWS.execRWS (mapM_ step es >> flush) () (M.empty, iterate (add binSize) minTime)
  where
    getBin       = RWS.gets $ \(m, t1:t2:ts) -> (t1, t2)
    nextBin      = RWS.get >>= \(m, t1:t2:ts) -> RWS.put (m, t2:ts)
    getState s t = RWS.gets $ \(m, _) -> (M.findWithDefault (0,t,0,0) s m)
    putState s v = RWS.get >>= \(m, ts) -> RWS.put (M.insert s v m, ts)
    modState s t f = getState s t >>= putState s . f
    getStates    = RWS.gets (\(m,_) -> M.toList m)

    flushBin = do
      bin@(t1,t2) <- getBin
      states <- getStates
      let binSizeSec = toSeconds (t2 `sub` t1) t1
      RWS.tell [(bin, [(s, (fromIntegral npulse + area + toSeconds (t2 `sub` start) t2*nopen)/binSizeSec) | (s,(area,start,nopen,npulse)) <- states])]
      forM_ states $ \(s, (area,start,nopen,_)) -> putState s (0,t2,nopen,0)
      nextBin

    step ev@(t, s, e) = do
      (t1, t2) <- getBin
      if t < t1
        then error "Times are not in ascending order"
        else if (t >= t2)
               then flushBin >> step ev
               else step'' ev
    step'' ev@(t,s,e) = do (t1,t2) <- getBin; when (t < t1 || t >= t2) (error "Outside bin"); step' ev
    step' (t, s, SetTo _) = modState s t id
    step' (t, s, Pulse _) = modState s t $ \(area, start, nopen, npulse) -> (area,                                   t, nopen,   npulse+1)
    step' (t, s, Rise)    = modState s t $ \(area, start, nopen, npulse) -> (area+toSeconds (t `sub` start) t*nopen, t, nopen+1, npulse)
    step' (t, s, Fall)    = modState s t $ \(area, start, nopen, npulse) -> (area+toSeconds (t `sub` start) t*nopen, t, nopen-1, npulse)
    flush                 = getBin >>= \(t1,t2) -> when (t2 <= maxTime) (flushBin >> flush)

values2timeBins :: (Ord t) => [t] -> [(t,a)] -> [[a]]
values2timeBins (t1:t2:ts) []          = []
values2timeBins (t1:t2:ts) tvs@((t,_):_)
  | t<t1 = error "Times are not in ascending order"
  | True = let (bin,rest) = span ((<t2).fst) tvs
           in (map snd bin : values2timeBins (t2:ts) rest)

byTimeBins :: (Ord t, HasDelta t, Ord a) => ([a] -> b) -> Delta t -> t -> [(t,a)] -> [(t, b)]
byTimeBins f binSize t0 tvs = times `zip` map f (values2timeBins times tvs)
  where times = iterate (add binSize) t0

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

edges2durations :: forall t . (Ord t, HasDelta t) => [(t,S.ByteString,Edge)] -> t -> t -> String -> [(t,InEvent)]
edges2durations tes minTime maxTime commonTrack = 
    [(t2, InValue commonTrackBS $ toSeconds (t2 `sub` t1) (undefined::t)) 
    | (track, LongEvent (t1,True) (t2,True) _) <- edges2events tes minTime maxTime]
 where commonTrackBS = S.pack commonTrack

edges2events :: (Ord t) => [(t,S.ByteString,Edge)] -> t -> t -> [(S.ByteString,Event t Status)]
edges2events tes minTime maxTime = snd $ RWS.execRWS (mapM_ step tes >> flush) () M.empty 
  where
    getTrack s = M.findWithDefault (minTime, 0, emptyStatus) s `fmap` RWS.get 
    putTrack s t = RWS.get >>= RWS.put . M.insert s t
    trackCase s whenZero withNonzero = do
      (t0, numActive, st) <- getTrack s
      case numActive of
        0 -> whenZero
        n -> withNonzero t0 numActive st
    killTrack s = RWS.get >>= RWS.put . M.delete s

    emptyStatus = Status "" ""

    step (t,s,Pulse st) = RWS.tell [(s, PulseEvent t st)]
    step (t,s,SetTo st) = trackCase s (putTrack s (t, 1, st))
                                      (\t0 n st0 -> RWS.tell [(s, LongEvent (t0,True) (t,True) st0)] >> 
                                                    putTrack s (t, n, st))
    step (t,s,Rise)     = trackCase s (putTrack s (t, 1, emptyStatus)) 
                                      (\t0 n st -> putTrack s (t, n+1, st))
    step (t,s,Fall)     = do
      (t0, numActive, st) <- getTrack s
      case numActive of
        1 -> RWS.tell [(s, LongEvent (t0,True) (t,True) st)] >> killTrack s
        n -> putTrack s (t0, max 0 (n-1), st)

    flush = RWS.get >>= mapM_ (\(s, (t0,_,st)) -> RWS.tell [(s, LongEvent (t0,True) (maxTime,False) st)]) . M.toList

-----------------------------------------------------


initGen KindLines            = genLines
initGen (KindDots     alpha) = genDots alpha
initGen (KindBinFreq  bs vs) = genBinFreqs bs vs
initGen (KindBinHist  bs vs) = genBinHist  bs vs
initGen (KindQuantile bs vs) = genQuantile bs vs

genValues (t,InValue s v) = Just (t,s,v)
genValues _               = Nothing

valuesDropTrack (t, InValue s v) = Just (t,v)
valuesDropTrack _                = Nothing

atomsDropTrack (t, InAtom s a) = Just (t,a)
atomsDropTrack _               = Nothing

genLines :: String -> LocalTime -> PlotGen
genLines name t0 = genFilterMap genValues $ listSummary (data2plot . groupByTrack)
  where
    data2plot vss = PlotLinesData {
        plotName = name,
        linesData = [vs | (_,vs) <- vss],
        linesStyles = [solidLine 1 color | _ <- vss | color <- map opaque colors],
        linesTitles = [S.unpack subtrack | (subtrack, _) <- vss]
      }

genDots :: Double -> String -> LocalTime -> PlotGen
genDots alpha name t0 = genFilterMap genValues $ listSummary (data2plot . groupByTrack)
  where
    data2plot vss = PlotDotsData {
        plotName = name,
        dotsData = [vs | (_,vs) <- vss],
        dotsTitles = [S.unpack subtrack | (subtrack, _) <- vss],
        dotsColors = if alpha == 1 then map opaque colors else map (`withOpacity` alpha) colors
      }

summaryByFixedTimeBins t0 binSize = summaryByTimeBins (iterate (add binSize) t0)

genByBins :: ([Double] -> [Double] -> [Double]) -> NominalDiffTime -> [Double] -> String -> LocalTime -> PlotGen
genByBins f binSize vs name t0 = genFilterMap valuesDropTrack $ 
    summaryByFixedTimeBins t0 binSize $
    mapInputSummary (\(t,xs) -> (t, 0:f vs xs)) $
    listSummary (\tfs -> plotTrackBars tfs (binTitles vs) name colors)

genBinHist :: NominalDiffTime -> [Double] -> String -> LocalTime -> PlotGen
genBinHist = genByBins values2binHist

genBinFreqs :: NominalDiffTime -> [Double] -> String -> LocalTime -> PlotGen
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

genQuantile :: NominalDiffTime -> [Double] -> String -> LocalTime -> PlotGen
genQuantile binSize qs name t0 = genFilterMap valuesDropTrack $
    summaryByFixedTimeBins t0 binSize $
    mapInputSummary (\(t,xs) -> (t,diffs (getQuantiles qs xs))) $
    listSummary (\tqs -> plotTrackBars tqs quantileTitles name colors)
  where
    quantileTitles = [""]++[show p1++".."++show p2++"%" | (p1,p2) <- lag percents ]
    percents = map (floor . (*100.0)) $ [0.0] ++ qs ++ [1.0]

genAtoms :: ([S.ByteString] -> M.Map S.ByteString Int -> [Double]) ->
           NominalDiffTime -> PlotBarsStyle -> String -> LocalTime -> PlotGen
genAtoms f binSize k name t0 = genFilterMap atomsDropTrack $
    mapOutputSummary (uncurry h) (teeSummary uniqueAtoms fInBins)
  where 
    fInBins :: StreamSummary (LocalTime, S.ByteString) [(LocalTime, M.Map S.ByteString Int)]
    fInBins = summaryByFixedTimeBins t0 binSize $
              mapInputSummary (\(t,as) -> (t, counts as)) $
              listSummary id
    counts  = foldl' insert M.empty
      where
        insert m a = case M.lookup a m of
          Nothing -> M.insert a 1     m
          Just !n -> M.insert a (n+1) m

    uniqueAtoms :: StreamSummary (LocalTime,S.ByteString) [S.ByteString]
    uniqueAtoms = mapInputSummary (\(t,a) -> a) $ statefulSummary M.empty (\a -> M.insert a ()) M.keys

    h :: [S.ByteString] -> [(LocalTime, M.Map S.ByteString Int)] -> PlotData
    h as tfs = plotTrackBars (map (\(t,counts) -> (t,f as counts)) tfs) (map show as) name colors

genSum :: NominalDiffTime -> SumSubtrackStyle -> String -> LocalTime -> PlotGen
genSum binSize ss name t0 = genFilterMap genValues $
    mapOutputSummary (uncurry h) (teeSummary uniqueSubtracks sumsInBins)
  where 
    sumsInBins :: StreamSummary (LocalTime,S.ByteString,Double) [(LocalTime, M.Map S.ByteString Double)]
    sumsInBins = mapInputSummary (\(t,s,v) -> (t,(s,v))) $
                 summaryByFixedTimeBins t0 binSize $
                 mapInputSummary (\(t,tvs) -> (t, fromListWith' (+) tvs)) $
                 listSummary id

    uniqueSubtracks :: StreamSummary (LocalTime,S.ByteString,Double) [S.ByteString]
    uniqueSubtracks = mapInputSummary (\(t,s,a) -> s) $ statefulSummary M.empty (\a -> M.insert a ()) M.keys

    h :: [S.ByteString] -> [(LocalTime, M.Map S.ByteString Double)] -> PlotData
    h tracks binSums = plotLines name rows
      where
        rowsT' = case ss of
          SumOverlayed -> map (\(t,ss) -> (t, M.toList ss)) binSums
          SumStacked   -> map (\(t,ss) -> (t, stack ss))    binSums

        stack :: M.Map S.ByteString Double -> [(S.ByteString, Double)]
        stack ss = zip tracks (scanl1 (+) (map (\x -> M.findWithDefault 0 x ss) tracks))
        
        rows :: [(S.ByteString, [(LocalTime, Double)])]
        rows = M.toList $ fmap sort $ M.fromListWith (++) $
          [(track, [(t,sum)]) | (t, m) <- rowsT', (track, sum) <- m]

fromListWith' :: (Ord k) => (a -> a -> a) -> [(k,a)] -> M.Map k a
fromListWith' f kvs = foldl' insert M.empty kvs
  where
    insert m (k,v) = case M.lookup k m of
      Nothing  -> M.insert k v        m
      Just !v' -> M.insert k (f v' v) m
