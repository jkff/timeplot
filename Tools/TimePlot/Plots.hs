{-# LANGUAGE ParallelListComp, ScopedTypeVariables, BangPatterns, Rank2Types, TupleSections #-}
module Tools.TimePlot.Plots (
    initGen
) where

import qualified Control.Monad.Trans.State.Strict as St
import Control.Arrow
import Control.Applicative
import Data.List (foldl', sort)
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
import qualified Tools.TimePlot.Incremental as I

type PlotGen = String -> LocalTime -> LocalTime -> I.StreamSummary (LocalTime, InEvent) PlotData

initGen :: ChartKind LocalTime -> PlotGen
initGen (KindACount bs)         = genActivity (\sns n -> n) bs
initGen (KindAPercent bs b)     = genActivity (\sns n -> 100*n/b) bs
initGen (KindAFreq bs)          = genActivity (\sns n -> if n == 0 then 0 else (n / sum (M.elems sns))) bs
initGen (KindFreq bs k)         = genAtoms atoms2freqs bs k
  where  atoms2freqs as m = let n = length as in [fromIntegral (M.findWithDefault 0 a m)/fromIntegral n | a <- as]
initGen (KindHistogram bs k)    = genAtoms atoms2hist bs k
  where  atoms2hist as m = [fromIntegral (M.findWithDefault 0 a m) | a <- as]
initGen KindEvent               = genEvent
initGen (KindQuantile bs vs)    = genQuantile bs vs
initGen (KindBinFreq  bs vs)    = genBinFreqs bs vs
initGen (KindBinHist  bs vs)    = genBinHist  bs vs
initGen KindLines               = genLines
initGen (KindDots     alpha)    = genDots alpha
initGen (KindSum bs ss)         = genSum bs ss
initGen (KindCumSum bs ss)      = genCumSum bs ss
initGen (KindDuration sk)       = genDuration sk
initGen (KindWithin _ _)        = \name -> error $ 
  "KindWithin should not be plotted (this is a bug): track " ++ show name
initGen KindNone                = \name -> error $ 
  "KindNone should not be plotted (this is a bug): track " ++ show name
initGen KindUnspecified         = \name -> error $ 
  "Kind not specified for track " ++ show name ++ " (have you misspelled -dk or any of -k arguments?)"

-- Auxiliary functions for two common plot varieties

plotTrackBars :: [(LocalTime,[Double])] -> [String] -> String -> [Colour Double] -> PlotData
plotTrackBars vals titles name colors = PlotBarsData {
        plotName = name,
        barsStyle = BarsStacked,
        barsValues = vals,
        barsStyles = [(solidFillStyle c, Nothing) 
                     |(i,_) <- [0..]`zip`titles 
                     | c <- transparent:map opaque colors],
        barsTitles = titles
    }

plotLines :: String -> [(S.ByteString, [(LocalTime,Double)])] -> PlotData
plotLines name vss = PlotLinesData {
        plotName = name,
        linesData = [vs | (_, vs) <- vss],
        linesStyles = [solidLine 1 color | _ <- vss | color <- map opaque colors],
        linesTitles = [S.unpack subtrack | (subtrack, _) <- vss]
    }

-------------------------------------------------------------
-- Plot generators
-------------------------------------------------------------

-- Wrappers for I.filterMap
values (t,InValue s v) = Just (t,s,v)
values _               = Nothing

valuesDropTrack (t, InValue s v) = Just (t,v)
valuesDropTrack _                = Nothing

atomsDropTrack (t, InAtom s a) = Just (t,a)
atomsDropTrack _               = Nothing

edges (t,InEdge s e) = Just (t,s,e)
edges _              = Nothing

------------------- Lines ----------------------
genLines :: PlotGen
genLines name t0 t1 = I.filterMap values $ (data2plot . groupByTrack) <$> I.collect
  where
    data2plot vss = PlotLinesData {
        plotName = name,
        linesData = [vs | (_,vs) <- vss],
        linesStyles = [solidLine 1 color | _ <- vss | color <- map opaque colors],
        linesTitles = [S.unpack subtrack | (subtrack, _) <- vss]
      }

------------------- Dots ----------------------
genDots :: Double -> PlotGen
genDots alpha name t0 t1 = I.filterMap values $ (data2plot . groupByTrack) <$> I.collect
  where
    data2plot vss = PlotDotsData {
        plotName = name,
        dotsData = [vs | (_,vs) <- vss],
        dotsTitles = [S.unpack subtrack | (subtrack, _) <- vss],
        dotsColors = if alpha == 1 then map opaque colors else map (`withOpacity` alpha) colors
      }

------------------- Binned graphs ----------------------
summaryByFixedTimeBins t0 binSize = I.byTimeBins (iterate (add binSize) t0)

-- Arguments of f will be: value bin boundaries, values in the current time bin
genByBins :: ([Double] -> [Double] -> [Double]) -> NominalDiffTime -> [Double] -> PlotGen
genByBins f timeBinSize valueBinBounds name t0 t1 = I.filterMap valuesDropTrack $ 
    summaryByFixedTimeBins t0 timeBinSize $
    I.mapInput (\(t,xs) -> (t, 0:f valueBinBounds xs)) $
    (\tfs -> plotTrackBars tfs binTitles name colors) <$>
    I.collect
  where
    binTitles = [low]++[show v1++".."++show v2 
                       | v1 <- valueBinBounds 
                       | v2 <- tail valueBinBounds]++
                [high]
      where
        low = "<"++show (head valueBinBounds)
        high = ">"++show (last valueBinBounds)

genBinHist :: NominalDiffTime -> [Double] -> PlotGen
genBinFreqs :: NominalDiffTime -> [Double] -> PlotGen
(genBinHist,genBinFreqs) = (genByBins values2binHist, genByBins values2binFreqs)
  where
    values2binHist bins = values2binHist' bins . sort

    values2binHist' []     xs = [fromIntegral (length xs)]
    values2binHist' (a:as) xs = fromIntegral (length xs0) : values2binHist' as xs'
      where (xs0,xs') = span (<a) xs

    values2binFreqs bins xs = map toFreq $ values2binHist bins xs
      where
        n = length xs
        toFreq k = if n==0 then 0 else (k/fromIntegral n)


genQuantile :: NominalDiffTime -> [Double] -> PlotGen
genQuantile binSize qs name t0 t1 = I.filterMap valuesDropTrack $
    summaryByFixedTimeBins t0 binSize $
    I.mapInput (second (diffs . getQuantiles qs)) $
    fmap (\tqs -> plotTrackBars tqs quantileTitles name colors) $
    I.collect
  where
    quantileTitles = [""]++[show p1++".."++show p2++"%" | p1 <- percents | p2 <- tail percents]
    percents = map (floor . (*100.0)) $ [0.0] ++ qs ++ [1.0]
    diffs xs = zipWith (-) xs (0:xs)

getQuantiles :: (Ord a) => [Double] -> [a] -> [a]
getQuantiles qs = quantiles' . sort
  where
    qs' = sort qs
    quantiles' [] = []
    quantiles' xs = index (0:ns++[n-1]) 0 xs
      where
        n  = length xs
        ns = map (floor . (*(fromIntegral n-1))) qs'

        index _   _ [] = []
        index []  _ _  = []
        index [i] j (x:xs)
          | i<j   = []
          | i==j  = [x]
          | True  = index [i] (j+1) xs
        index (i:i':is) j (x:xs)
          | i<j   = index (i':is)   j     (x:xs)
          | i>j   = index (i:i':is) (j+1) xs
          | i==i' = x:index (i':is) j     (x:xs)
          | True  = x:index (i':is) (j+1) xs

genAtoms :: ([S.ByteString] -> M.Map S.ByteString Int -> [Double]) ->
           NominalDiffTime -> PlotBarsStyle -> PlotGen
genAtoms f binSize k name t0 t1 = I.filterMap atomsDropTrack (h <$> unique (\(t,atom) -> atom) <*> fInBins)
  where 
    fInBins :: I.StreamSummary (LocalTime, S.ByteString) [(LocalTime, M.Map S.ByteString Int)]
    fInBins = summaryByFixedTimeBins t0 binSize $ I.mapInput (second counts) I.collect
    counts  = foldl' insert M.empty
      where
        insert m a = case M.lookup a m of
          Nothing -> M.insert a 1     m
          Just !n -> M.insert a (n+1) m

    h :: [S.ByteString] -> [(LocalTime, M.Map S.ByteString Int)] -> PlotData
    h as tfs = plotTrackBars (map (second (f as)) tfs) (map show as) name colors

unique :: (Ord a) => (x -> a) -> I.StreamSummary x [a]
unique f = I.stateful M.empty (\a -> M.insert (f a) ()) M.keys

uniqueSubtracks :: I.StreamSummary (LocalTime,S.ByteString,a) [S.ByteString]
uniqueSubtracks = unique (\(t,s,a) -> s)

genSum :: NominalDiffTime -> SumSubtrackStyle -> PlotGen
genSum binSize ss name t0 t1 = I.filterMap values (h <$> uniqueSubtracks <*> sumsInBins t0 binSize)
  where 
    h :: [S.ByteString] -> [(LocalTime, M.Map S.ByteString Double)] -> PlotData
    h tracks binSums = plotLines name rows
      where
        rowsT' = case ss of
          SumOverlayed -> map (second M.toList) binSums
          SumStacked   -> map (second stack)    binSums

        stack :: M.Map S.ByteString Double -> [(S.ByteString, Double)]
        stack ss = zip tracks (scanl1 (+) (map (\x -> M.findWithDefault 0 x ss) tracks))
        
        rows :: [(S.ByteString, [(LocalTime, Double)])]
        rows = M.toList $ fmap sort $ M.fromListWith (++) $
          [(track, [(t,sum)]) | (t, m) <- rowsT', (track, sum) <- m]

sumsInBins :: LocalTime -> NominalDiffTime -> I.StreamSummary (LocalTime,S.ByteString,Double) [(LocalTime, M.Map S.ByteString Double)]
sumsInBins t0 bs = I.mapInput (\(t,s,v) -> (t,(s,v))) $
                   summaryByFixedTimeBins t0 bs $
                   I.mapInput (second (fromListWith' (+))) $
                   I.collect

genCumSum :: NominalDiffTime -> SumSubtrackStyle -> PlotGen
genCumSum bs ss name t0 t1 = I.filterMap values (accumulate <$> uniqueSubtracks <*> sumsInBins t0 bs)
  where
    accumulate :: [S.ByteString] -> [(LocalTime, M.Map S.ByteString Double)] -> PlotData
    accumulate tracks tss = plotLines name [(track, [(t, ss M.! track) | (t,ss) <- cumsums]) | track <- tracks]
      where 
        cumsums = scanl' f (t0, M.fromList $ zip tracks (repeat 0)) (map normalize tss)
        normalize (t,binSums) = (t, M.fromList [ (track, M.findWithDefault 0 track binSums) | track <- tracks ])

        f (_,bases) (t,binSums) = (t,) $ M.fromList $ zip tracks $ zipWith (+) trackBases $ case ss of 
            SumOverlayed -> trackSums
            SumStacked   -> trackAccSums
          where
            trackSums    = [ binSums M.! track | track <- tracks ]
            trackBases   = [ bases   M.! track | track <- tracks ]
            trackAccSums = scanl1' (+) trackSums
scanl1' f (x:xs) = scanl' f x xs
scanl' f !x0 [] = [x0]
scanl' f !x0 (x:xs) = x0:scanl' f (f x0 x) xs

genActivity :: (M.Map S.ByteString Double -> Double -> Double) -> NominalDiffTime -> PlotGen
genActivity f bs name t0 t1 = I.filterMap edges (h <$> uniqueSubtracks <*> binAreas)
  where 
    binAreas :: I.StreamSummary (LocalTime,S.ByteString,Edge) [(LocalTime, M.Map S.ByteString Double)]
    binAreas = fmap (map (\((t1,t2),m) -> (t1,m))) $ edges2binsSummary bs t0 t1

    h tracks binAreas = (plotTrackBars barsData ("":map S.unpack tracks) name colors) { barsStyle = BarsStacked }
      where
        barsData = [(t, 0:map (f m . flip (M.findWithDefault 0) m) tracks) | (t,m) <- binAreas]

edges2binsSummary :: (Ord t,HasDelta t,Show t) => 
    Delta t -> t -> t -> 
    I.StreamSummary (t,S.ByteString,Edge) [((t,t), M.Map S.ByteString Double)]
edges2binsSummary binSize tMin tMax = I.stateful (M.empty, iterate (add binSize) tMin, []) step flush
  where
    -- State: (m, ts, r) where:
    --  * m  = subtrack => state of current bin: 
    --    (area, starting time, level = rise-fall, num pulse events)
    --  * ts = infinite list of time bin boundaries
    --  * r  = reversed list of results per bins
    modState s t (!m, ts,r) f = (m', ts, r)
      where
        m' = M.insertWith' (\new !old -> f old) s (f (0,t,0,0)) m

    flushBin st@(m,t1:t2:ts,!r) = (m', t2:ts, r')
      where
        states = M.toList m
        binSizeSec = deltaToSeconds t2 t1
        binValue (area,start,nopen,npulse) = 
          (fromIntegral npulse + area + deltaToSeconds t2 start*nopen) / binSizeSec
        !r' = ((t1,t2), M.fromList [(s, binValue bin) | (s, bin) <- states]) : r
        !m' = fmap (\(_,_,nopen,_) -> (0,t2,nopen,0)) m

    step ev@(t, s, e) st@(m, t1:t2:ts, r)
      | t < t1  = error "Times are not in ascending order"
      | t >= t2 = step ev (flushBin st)
      | True    = step'' ev st

    step'' ev@(t,s,e) st@(m, t1:t2:ts, r) = if (t < t1 || t >= t2) then error "Outside bin" else step' ev st
    step' (t, s, SetTo _) st = st
    step' (t, s, Pulse _) st = modState s t st $ 
      \(!area, !start, !nopen, !npulse) -> (area,                                   t, nopen,   npulse+1)
    step' (t, s, Rise)    st = modState s t st $ 
      \(!area, !start, !nopen, !npulse) -> (area+deltaToSeconds t start*nopen, t, nopen+1, npulse)
    step' (t, s, Fall)    st = modState s t st $ 
      \(!area, !start, !nopen, !npulse) -> (area+deltaToSeconds t start*nopen, t, nopen-1, npulse)
    flush st@(m, t1:t2:ts, r) 
      | t2 <= tMax = flush (flushBin st)
      | True       = reverse r

type StreamTransformer a b = forall r . I.StreamSummary b r -> I.StreamSummary a r

edges2eventsSummary :: forall t . (Ord t) => 
    t -> t -> StreamTransformer (t,S.ByteString,Edge) (S.ByteString, Event t Status)
edges2eventsSummary t0 t1 s = I.stateful (M.empty,s) step flush
  where
    -- State: (m, sum) where
    --  * m = subtrack => (event start, level = rise-fall, status)
    --  * sum = summary of accumulated events so far
    tellSummary e (ts,!sum) = (ts,I.insert sum e)

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

    flush (ts,sum) = I.finalize $ foldl' addEvent sum $ M.toList ts
      where
        addEvent sum (s,(t0,_,st)) = I.insert sum (s, LongEvent (t0,True) (t1,False) st)

edges2durationsSummary :: forall t . (Ord t, HasDelta t) => 
    t -> t -> String -> StreamTransformer (t,S.ByteString,Edge) (t,InEvent)
edges2durationsSummary t0 t1 commonTrack = edges2eventsSummary t0 t1 . I.filterMap genDurations
  where
    genDurations (track, e) = case e of
      LongEvent (t1,True) (t2,True) _ -> Just (t2, InValue commonTrackBS $ deltaToSeconds t2 t1)
      _                               -> Nothing
    commonTrackBS = S.pack commonTrack

genEvent :: PlotGen
genEvent name t0 t1 = I.filterMap edges $ 
                      fmap (\evs -> PlotEventData { plotName = name, eventData = map snd evs }) $ 
                      edges2eventsSummary t0 t1 I.collect
-- TODO Multiple tracks

genDuration :: ChartKind LocalTime -> PlotGen
genDuration sk name t0 t1 = I.filterMap edges $ edges2durationsSummary t0 t1 name (initGen sk name t0 t1)

fromListWith' f kvs = foldl' insert M.empty kvs
  where
    insert m (k,v) = case M.lookup k m of
      Nothing  -> M.insert k v        m
      Just !v' -> M.insert k (f v' v) m

colors = cycle [green,blue,red,brown,yellow,orange,grey,purple,violet,lightblue]

groupByTrack xs = M.toList $ sort `fmap` M.fromListWith (++) [(s, [(t,v)]) | (t,s,v) <- xs]

