{-# LANGUAGE ParallelListComp, ScopedTypeVariables #-}
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

plotTrackAtoms :: ([S.ByteString] -> [S.ByteString] -> [Double]) ->
                  String -> [(LocalTime,InEvent)] -> LocalTime -> NominalDiffTime -> PlotBarsStyle -> PlotData
plotTrackAtoms f name es minInTime bs k = PlotBarsData {
        plotName = name,
        barsStyle = k,
        barsValues = vals,
        barsStyles = itemStyles,
        barsTitles = map show vs
    }
  where itemStyles = none:[(solidFillStyle (opaque c), Nothing) | c <- colors]
        vals = byTimeBins ((0:).f vs) bs minInTime as
        -- TODO Multiple tracks
        as   = [(t,a) | (t,_,a) <- atoms es]
        vs   = M.keys $ M.fromList $ [(a,()) | (_,a) <- as]

-- TODO Multiple tracks
plotTrackEvent :: String -> [(LocalTime,InEvent)] -> LocalTime -> LocalTime -> PlotData
plotTrackEvent name es minInTime maxInTime = PlotEventData { 
        plotName = name,
        eventData = dropTrack (edges2events (edges es) minInTime maxInTime)
    }
  where dropTrack = map snd
        toFillStyle s = solidFillStyle . opaque $ fromMaybe lightgray (readColourName (statusColor s))
        toLabel     s = statusLabel s

plotTrackQuantile :: String -> [(LocalTime,InEvent)] -> LocalTime -> [Double] -> NominalDiffTime -> PlotData
plotTrackQuantile  name es minInTime qs bs = PlotBarsData {
        plotName = name,
        barsStyle = BarsStacked,
        barsValues = toBars (byTimeBins (getQuantiles qs) bs minInTime vs),
        barsStyles = quantileStyles,
        barsTitles = quantileTitles
    }
  where -- TODO Multiple tracks
        vs = [(t,v) | (t,_,v) <- values es]
        quantileStyles = none:(zip (map (solidFillStyle . opaque) colors) [Nothing | i <- [0..n+1]])
        quantileTitles = [""]++[show p1++".."++show p2++"%" | (p1,p2) <- lag percents ]
          where
            percents = map (floor . (*100.0)) $ [0.0] ++ qs ++ [1.0]
        n = length qs

plotTrackBinFreqs  name es minInTime vs bs = runStreamSummary (genBinFreqs minInTime bs vs name) es
plotTrackBinHist   name es minInTime vs bs = plotTrackBars vals (binTitles vs) name (binColor n)
  where
    vals = byTimeBins ((0:).values2binHist vs) bs minInTime tvs
    tvs  = [(t,v) | (t,_,v) <- values es]
    n    = length vs

plotTrackBars :: [(LocalTime,[Double])] -> [String] -> String -> (Int -> AlphaColour Double) -> PlotData
plotTrackBars values titles name clr = PlotBarsData {
        plotName = name,
        barsStyle = BarsStacked,
        barsValues = values,
        barsStyles = none:[(solidFillStyle (clr i), Nothing) | (i,_) <- [0..]`zip`titles],
        barsTitles = titles
    }

none = (solidFillStyle transparent, Nothing)

plotTrackLines :: String -> [(LocalTime,InEvent)] -> PlotData
plotTrackLines name es = runStreamSummary (genLines name) es

plotTrackDots :: String -> [(LocalTime,InEvent)] -> Double -> PlotData
plotTrackDots  name es alpha = runStreamSummary (genDots alpha name) es

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

plotTrackSum :: String -> [(LocalTime,InEvent)] -> LocalTime -> NominalDiffTime -> SumSubtrackStyle -> PlotData
plotTrackSum name es minInTime bs ss = plotLines name rows
  where groups    = groupByTrack (values es)
        allTracks = M.keys $ M.fromList groups
        
        rowsT :: [(LocalTime, M.Map S.ByteString Double)]
        rowsT = byTimeBins (M.fromListWith (+)) bs minInTime $ sort [(t, (track, v)) | (track, vs) <- groups, (t, v) <- vs]
        
        rowsT' = case ss of
          SumOverlayed -> map (\(t,ss) -> (t, M.toList ss)) rowsT
          SumStacked   -> map (\(t,ss) -> (t, stack ss))    rowsT

        stack :: M.Map S.ByteString Double -> [(S.ByteString, Double)]
        stack ss = zip allTracks (scanl1 (+) (map (\x -> M.findWithDefault 0 x ss) allTracks))
        
        rows :: [(S.ByteString, [(LocalTime, Double)])]
        rows  = M.toList $ sort `fmap` M.fromListWith (++) [(track, [(t,sum)]) | (t, m) <- rowsT', (track, sum) <- m]

edges  :: [(LocalTime,InEvent)] -> [(LocalTime,S.ByteString,Edge)]
values :: [(LocalTime,InEvent)] -> [(LocalTime,S.ByteString,Double)]
atoms  :: [(LocalTime,InEvent)] -> [(LocalTime,S.ByteString,S.ByteString)]
edges  es = [(t,s,e) | (t,InEdge  s e) <- es]
values es = [(t,s,v) | (t,InValue s v) <- es]
atoms  es = [(t,s,a) | (t,InAtom  s a) <- es]

lag :: [a] -> [(a,a)]
lag xs = xs `zip` tail xs

colors = cycle [green,blue,red,brown,yellow,orange,grey,purple,violet,lightblue]

binTitles vs = [low]++[show v1++".."++show v2 | (v1,v2) <- lag vs]++[high]
  where
    low = "<"++show (head vs)
    high = ">"++show (last vs)

binColor n i = opaque (colors !! i)

toBars tvs = [(t,diffs vs) | (t,vs) <- tvs]
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

atoms2hist :: (Ord a) => [a] -> [a] -> [Double]
atoms2hist as xs = map (maybe 0 id . (`M.lookup` m)) as
  where
    m          = foldl' insert M.empty xs
    insert m a = M.alter (Just . maybe 1 inc) a m
    inc    n   = n `seq` (n+1)

atoms2freqs :: (Ord a) => [a] -> [a] -> [Double]
atoms2freqs as xs = map toFreq (atoms2hist as xs)
  where
    n = length xs
    toFreq = if n==0 then const 0 else (\k -> k/fromIntegral n)

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


initGen KindLines = genLines
initGen (KindDots alpha) = genDots alpha

genValues (t,InValue s v) = Just (t,s,v)
genValues _               = Nothing

genLines :: String -> PlotGen
genLines name = genFilterMap genValues $ listSummary (data2plot . groupByTrack)
  where
    data2plot vss = PlotLinesData {
        plotName = name,
        linesData = [vs | (_,vs) <- vss],
        linesStyles = [solidLine 1 color | _ <- vss | color <- map opaque colors],
        linesTitles = [S.unpack subtrack | (subtrack, _) <- vss]
      }

genDots :: Double -> String -> PlotGen
genDots alpha name = genFilterMap genValues $ listSummary (data2plot . groupByTrack)
  where
    data2plot vss = PlotDotsData {
        plotName = name,
        dotsData = [vs | (_,vs) <- vss],
        dotsTitles = [S.unpack subtrack | (subtrack, _) <- vss],
        dotsColors = if alpha == 1 then map opaque colors else map (`withOpacity` alpha) colors
      }

genBinFreqs :: LocalTime -> NominalDiffTime -> [Double] -> String -> PlotGen
genBinFreqs t0 binSize vs name = genFilterMap genValuesDropTrack $ summaryByTimeBins bins f
  where
    f :: StreamSummary (LocalTime,[Double]) PlotData
    f = mapInputSummary (\(t,xs) -> (t, 0:values2binFreqs vs xs)) plotSummary

    plotSummary :: StreamSummary (LocalTime,[Double]) PlotData
    plotSummary = listSummary (\tfs -> plotTrackBars tfs (binTitles vs) name (binColor n))

    bins = iterate (add binSize) t0
    n    = length vs
    -- TODO Multiple tracks
    genValuesDropTrack (t, InValue s v) = Just (t,v)
    genValuesDropTrack _                = Nothing

