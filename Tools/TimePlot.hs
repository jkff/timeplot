{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts, GADTs, CPP, ParallelListComp #-}
module Main where

import Control.Monad
import qualified Control.Monad.Trans.State.Strict as St
import qualified Control.Monad.Trans.RWS.Strict as RWS
import Control.Arrow
import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lex.Lazy.Double

import Data.Char

import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString

import System
import System.Exit
import System.Console.GetOpt

import Data.Time hiding (parseTime)
import Data.Time.Parse

import Data.Accessor

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Plot
import Graphics.Rendering.Chart.Event

import Data.Colour
import Data.Colour.Names

import Debug.Trace

data Status = Status {statusColor :: String, statusLabel :: String} deriving (Eq, Show, Ord)

instance PlotValue Status where
  toValue = const 0
  fromValue = const (Status "" "")
  autoAxis = const unitStatusAxis

unitStatusAxis :: AxisData Status
unitStatusAxis = AxisData {
    axis_viewport_ = \(x0,x1) _ -> (x0+x1)/2,
    axis_tropweiv_ = \_       _ -> Status "" "",
    axis_ticks_    = [(Status "" "", 0)],
    axis_labels_   = [[(Status "" "", "")]],
    axis_grid_     = []
}

data Edge = Rise | Fall | Pulse Status | SetTo Status deriving (Eq,Show)

data InEvent = InEdge  {evt_track :: S.ByteString, evt_edge :: Edge}
             | InValue {evt_track :: S.ByteString, evt_value :: Double}
             | InAtom  {evt_track :: S.ByteString, evt_atom :: S.ByteString}
             deriving (Show)

data OutFormat = PNG | PDF | PS | SVG
               | Window

class HasDelta t where
  type Delta t :: *
  add :: Delta t -> t -> t
  sub :: t -> t -> Delta t
  -- the 't' is a dummy argument here, just to aid type checking
  -- (since given just a Delta t, the compiler won't be able to
  -- figure out which 't' we're speaking of)
  toSeconds :: Delta t -> t -> Double
  fromSeconds :: Double -> t -> Delta t
  showDelta :: t -> t -> String

instance HasDelta Double where
  type Delta Double = Double
  add d t = t + d
  sub t2 t1 = t2 - t1
  toSeconds d _ = d
  fromSeconds d _ = d
  showDelta a b = show (a - b)

instance HasDelta LocalTime where
  type Delta LocalTime = NominalDiffTime
  add d t = utcToLocalTime utc (addUTCTime d (localTimeToUTC utc t))
  sub t2 t1 = diffUTCTime (localTimeToUTC utc t2) (localTimeToUTC utc t1)
  toSeconds d _ = fromIntegral (truncate (1000000*d)) / 1000000
  fromSeconds d _ = fromRational (toRational d)
  showDelta t1 t2
    | ts0 < 0.001 = "0"
    | tm < 1 = showsPrec 3 s "s"
    | th < 1 = show m ++ "m" ++ (if s<1 then "" else (show (floor s) ++ "s"))
    | d  < 1 = show h ++ "h" ++ (if m<1 then "" else (show m ++ "m"))
    | True   = show d ++ "d" ++ (if h<1 then "" else (show h ++ "h"))
    where ts0 = toSeconds (t1 `sub` t2) t1
          ts = if ts0 < 60 then ts0 else fromIntegral (round ts0)
          tm = floor (ts / 60) :: Int
          th = tm `div` 60 :: Int
          s = ts - 60 * fromIntegral tm :: Double
          m = tm - 60 * th :: Int
          h = th - 24 * d :: Int
          d = h `div` 24 :: Int

instance Read NominalDiffTime where
  readsPrec n s = [(fromSeconds i (undefined::LocalTime), s') | (i,s') <- readsPrec n s]

class (Ord t, HasDelta t, PlotValue t, Show t, Show (Delta t), Read (Delta t)) => TimeAxis t

instance TimeAxis Double

instance TimeAxis LocalTime

data SumSubtrackStyle = SumStacked | SumOverlayed

data ChartKind t = KindEvent
               | KindDuration  { subKind :: ChartKind t }
               | KindWithin    { mapName :: S.ByteString -> S.ByteString, subKind :: ChartKind t }
               | KindACount    { binSize :: Delta t }
               | KindAPercent  { binSize :: Delta t, baseCount :: Double }
               | KindAFreq     { binSize :: Delta t }
               | KindQuantile  { binSize :: Delta t, quantiles :: [Double] }
               | KindBinFreq   { binSize :: Delta t, delims    :: [Double] }
               | KindBinHist   { binSize :: Delta t, delims    :: [Double] }
               | KindFreq      { binSize :: Delta t, style :: PlotBarsStyle }
               | KindHistogram { binSize :: Delta t, style :: PlotBarsStyle }
               | KindLines
               | KindDots
               | KindCumSum    { subtrackStyle :: SumSubtrackStyle }
               | KindSum       { binSize :: Delta t, subtrackStyle :: SumSubtrackStyle }
               | KindNone

data ZoomMode = ZoomInput | ZoomOutput

data ConcreteConf t =
  ConcreteConf {
    inFile        :: FilePath,
    parseTime     :: B.ByteString -> Maybe (t, B.ByteString),
    chartKindF    :: S.ByteString -> [ChartKind t],

    fromTime      :: Maybe t,
    toTime        :: Maybe t,
    zoomMode      :: ZoomMode,
    transformLabel :: t -> String -> String,

    outFile       :: FilePath,
    outFormat     :: OutFormat,
    outResolution :: (Int,Int)
  }

data Conf = forall t . (TimeAxis t) => Conf {concrete :: ConcreteConf t}

data KindChoiceOperator = Cut | Accumulate

readConf :: [String] -> Conf
readConf args = case (words $ single "time format" "-tf" ("date %Y-%m-%d %H:%M:%OS")) of
    ["num"]  -> Conf $ readConf' readDouble
    "date":f -> Conf $ readConf' (strptime (B.pack $ unwords f))
    _        -> error "Unrecognized time format (-tf)"
  where
    int2double = fromIntegral :: Int -> Double
    single desc name def = case (getArg name 1 args) of
      [[r]] -> r
      []    -> def
      _     -> error $ "Single argument expected for: "++desc++" ("++name++")"

    readConf' :: forall t. (TimeAxis t) => (B.ByteString -> Maybe (t, B.ByteString)) -> ConcreteConf t
    readConf' parseTime = ConcreteConf {inFile=inFile, outFile=outFile, outFormat=outFormat, outResolution=outRes,
                      chartKindF=chartKindF, parseTime=parseTime, fromTime=fromTime, toTime=toTime,
                      transformLabel=transformLabel, zoomMode=zoomMode}
      where
        inFile      = single "input file"  "-if" (error "No input file (-if) specified")
        outFile     = single "output file" "-o"  (error "No output file (-o) specified")
        outFormat   = maybe PNG id $ lookup (single "output format" "-of" (name2format outFile)) $
            [("png",PNG), ("pdf",PDF), ("ps",PS), ("svg",SVG)
            , ("x",Window)
            ]
          where
            name2format = reverse . takeWhile (/='.') . reverse
        outRes      = parseRes $ single "output resolution" "-or" "640x480"
          where
            parseRes s = case break (=='x') s of (h,_:v) -> (read h,read v)
        chartKindF  = kindByRegex $
            [(Cut,        matches regex, parseKind (words kind)) | [regex,kind] <- getArg "-k" 2 args] ++
            [(Accumulate, matches regex, parseKind (words kind)) | [regex,kind] <- getArg "+k" 2 args]
          where
            ifNull xs y = case xs of { [] -> [y] ; _ -> xs }
            kindByRegex rks s = (defaultKindsPlus ++
                                [k | (Accumulate, p, k) <- rks, p s] ++
                                [case [k | (Cut, p, k) <- rks, p s] of { [] -> defaultKindMinus; k:_ -> k }])
            matches regex = matchTest (makeRegexOpts defaultCompOpt (ExecOption {captureGroups = False}) regex)

        fromTime    = fst `fmap` (parseTime . B.pack $ single "minimum time (inclusive)" "-fromTime" "")
        toTime      = fst `fmap` (parseTime . B.pack $ single "maximum time (exclusive)" "-toTime"   "")
        baseTime    = fst `fmap` (parseTime . B.pack $ single "base time"                "-baseTime" "")

        zoomMode = case single "zoom mode (input/output)" "-zoomMode" "input" of
          "input"  -> ZoomInput
          "output" -> ZoomOutput

        transformLabel t s = case baseTime of
          Nothing -> s
          Just bt -> showDelta t bt

        parseKind ["acount",  n  ] = KindACount    {binSize=read n}
        parseKind ["apercent",n,b] = KindAPercent  {binSize=read n,baseCount=read b}
        parseKind ["afreq",   n  ] = KindAFreq     {binSize=read n}
        parseKind ["freq",    n  ] = KindFreq      {binSize=read n,style=BarsClustered}
        parseKind ["freq",    n,s] = KindFreq      {binSize=read n,style=parseStyle s}
        parseKind ["hist",    n  ] = KindHistogram {binSize=read n,style=BarsClustered}
        parseKind ["hist",    n,s] = KindHistogram {binSize=read n,style=parseStyle s}
        parseKind ["event"       ] = KindEvent
        parseKind ["quantile",b,q] = KindQuantile  {binSize=read b, quantiles=read ("["++q++"]")}
        parseKind ["binf",    b,q] = KindBinFreq   {binSize=read b, delims   =read ("["++q++"]")}
        parseKind ["binh",    b,q] = KindBinHist   {binSize=read b, delims   =read ("["++q++"]")}
        parseKind ["lines"       ] = KindLines
        parseKind ["dots"        ] = KindDots
        parseKind ["cumsum"      ] = KindCumSum    {subtrackStyle=SumStacked}
        parseKind ["cumsum",  s  ] = KindCumSum    {subtrackStyle=parseSubtrackStyle s}
        parseKind ["sum",     b  ] = KindSum       {binSize=read b, subtrackStyle=SumStacked}
        parseKind ["sum",     b,s] = KindSum       {binSize=read b, subtrackStyle=parseSubtrackStyle s}
        parseKind ("duration":ws)  = KindDuration  {subKind=parseKind ws}
        parseKind (('w':'i':'t':'h':'i':'n':'[':sep:"]"):ws)
                                   = KindWithin    {subKind=parseKind ws, mapName = fst . S.break (==sep)}
        parseKind ["none"        ] = KindNone
        parseKind ws               = error ("Unknown diagram kind " ++ unwords ws)

        defaultKindMinus = parseKind $ words $ single "default kind" "-dk" "none"
        defaultKindsPlus = map (parseKind . words . head) $ getArg "+dk" 1 args

        parseStyle "stacked"   = BarsStacked
        parseStyle "clustered" = BarsClustered

        parseSubtrackStyle "stacked"   = SumStacked
        parseSubtrackStyle "overlayed" = SumOverlayed


-- getArg "-a" 2 ["-b", "1", "-a", "2", "q", "r", "-c", "3", "-a", "x"] =
-- [["2", "q"], ["x"]]
getArg :: String -> Int -> [String] -> [[String]]
getArg name arity args = [take arity as | (t:as) <- tails args, t==name]

readSource :: (Show t) => (B.ByteString -> Maybe (t,B.ByteString)) -> FilePath -> IO [(t, InEvent)]
readSource readTime f = (justs . map parseLine . blines) `fmap` (if f=="-" then B.getContents else B.readFile f)
  where
    justs xs = [x | Just x <- xs]
    blines   = map pruneLF . B.split '\n'
    pruneLF b | not (B.null b) && (B.last b == '\r') = B.init b
              | otherwise                            = b
    strict   = S.concat . B.toChunks
    parseLine s = do
      (t, s') <- readTime s
      (_, s'') <- B.uncons s'
      (c,rest) <- B.uncons s''
      case c of
        '>' -> return (t, InEdge (strict rest) Rise )
        '<' -> return (t, InEdge (strict rest) Fall )
        '!' -> do
          let (track, val') = B.break (==' ') rest
          if B.null val'
            then return (t, InEdge (strict track) (Pulse (Status "" "")))
            else do
              (_,val) <- B.uncons val'
              return (t, InEdge (strict track) $ Pulse (Status "" (B.unpack val)))
        '@' -> do
          let (track, val') = B.break (==' ') rest
          (_,val) <- B.uncons val'
          return (t, InEdge (strict track) $ SetTo (Status {statusColor = B.unpack $ val, statusLabel = ""}))
        '=' -> do
          let (track, val') = B.break (==' ') rest
          (_,val) <- B.uncons val'
          if B.null val
            then Nothing
            else do
              case B.head val of
                '`' -> do
                  return (t, InAtom (strict track) (strict $ B.tail val))
                _   -> do
                  (v,_  ) <- readDouble val
                  return (t, InValue (strict track) v)
        _   -> Nothing

makeChart :: forall t . TimeAxis t =>
             (S.ByteString -> [ChartKind t]) -> 
             [(t, InEvent)] ->
             Maybe t -> Maybe t ->
             ZoomMode -> 
             (t -> String -> String) -> 
             Renderable ()
makeChart chartKindF []      minT maxT zoomMode transformLabel = emptyRenderable
makeChart chartKindF events0 minT maxT zoomMode transformLabel = renderLayout1sStacked plots
  where
    events :: [(t, InEvent)]
    events@((t0,_):_) = sortBy (comparing (\(t,_)-> t)) events0

    track2events :: M.Map S.ByteString [(t, InEvent)]
    track2events = reverse `fmap` foldl' insert M.empty [(evt_track e, x) | x@(t, e) <- events]
      where insert m (s, r) = M.alter (Just . maybe [r] (r:)) s m

    plots          = [ plotTrack k kind es | (k, es) <- M.toList track2events,
                                             kind <- chartKindF k,
                                             case kind of {KindNone -> False ; KindWithin _ _ -> False ; _ -> True} ] ++
                     withinPlots

    withinPlots  = [ plotWithKind name k es | (name, (k,es)) <- M.toList withinTracks ]
      where
        withinTracks = M.fromListWith (\(ka,as) (kb,bs) -> (ka,mergeOn fst as bs)) components
        components = [ (mn k, (sk, es))
                     | (k, es) <- M.toList track2events,
                       kind <- chartKindF k,
                       Just (sk,mn) <- [case kind of {KindWithin mn sk -> Just (sk,mn) ; _ -> Nothing}]]
        mergeOn f [] ys = ys
        mergeOn f xs [] = xs
        mergeOn f (x:xs) (y:ys)
          | f x <= f y = x : mergeOn f xs (y:ys)
          | otherwise  = y : mergeOn f (x:xs) ys

    minInTime  = case (zoomMode, minT) of (ZoomInput,  Just t) -> t ; _ -> head times
    maxInTime  = case (zoomMode, maxT) of (ZoomInput,  Just t) -> t ; _ -> last times

    minOutTime = case (zoomMode, minT) of (ZoomOutput, Just t) -> t ; _ -> head times
    maxOutTime = case (zoomMode, maxT) of (ZoomOutput, Just t) -> t ; _ -> last times

    times             :: [t]
    times             = sort $ [t | tes <- M.elems track2events, (t,_)<- tes]

    commonTimeAxis    :: AxisData t
    commonTimeAxis    = transformLabels $ autoAxis axisTimes
      where
        axisTimes = case zoomMode of
          ZoomInput  -> [minInTime] ++ times ++ [maxInTime]
          ZoomOutput -> [minOutTime] ++ filter (\x -> x >= minOutTime && x <= maxOutTime) times ++ [maxOutTime]
        transformLabels axis = axis { axis_labels_ = map (map (\(t, s) -> (t, transformLabel t s))) (axis_labels_ axis) }

    plotTrack :: S.ByteString -> ChartKind t -> [(t, InEvent)] -> AnyLayout1 t
    plotTrack name kind es = plotWithKind name kind es

    plotWithKind :: S.ByteString -> ChartKind t -> [(t, InEvent)] -> AnyLayout1 t
    plotWithKind name k es = case k of
      KindACount    bs    -> withAnyOrdinate $ plotTrackACount    name es bs
      KindAPercent  bs b  -> withAnyOrdinate $ plotTrackAPercent  name es bs b
      KindAFreq     bs    -> withAnyOrdinate $ plotTrackAFreq     name es bs
      KindFreq      bs k  -> withAnyOrdinate $ plotTrackFreq      name es bs k
      KindHistogram bs k  -> withAnyOrdinate $ plotTrackHist      name es bs k
      KindEvent           -> withAnyOrdinate $ plotTrackEvent     name es
      KindQuantile  bs qs -> withAnyOrdinate $ plotTrackQuantile  name es qs bs
      KindBinFreq   bs vs -> withAnyOrdinate $ plotTrackBinFreqs  name es vs bs
      KindBinHist   bs vs -> withAnyOrdinate $ plotTrackBinHist   name es vs bs
      KindLines           -> withAnyOrdinate $ plotTrackLines     name es
      KindDots            -> withAnyOrdinate $ plotTrackDots      name es
      KindSum       bs ss -> withAnyOrdinate $ plotTrackSum       name es bs ss
      KindCumSum    ss    -> withAnyOrdinate $ plotTrackCumSum    name es ss
      KindDuration  sk    -> plotWithKind       name sk (edges2durations (edges es) minInTime maxInTime name)
      KindWithin    _ _   -> error "KindDuration should not be plotted"
      KindNone            -> error "KindNone should not be plotted"

    edges  :: [(t,InEvent)] -> [(t,S.ByteString,Edge)]
    values :: [(t,InEvent)] -> [(t,S.ByteString,Double)]
    atoms  :: [(t,InEvent)] -> [(t,S.ByteString,S.ByteString)]
    edges  es = [(t,s,e) | (t,InEdge  s e) <- es]
    values es = [(t,s,v) | (t,InValue s v) <- es]
    atoms  es = [(t,s,a) | (t,InAtom  s a) <- es]

    ourPlotBars :: (BarsPlotValue a) => PlotBars t a
    ourPlotBars = plot_bars_spacing ^= BarsFixGap 0 0 $
                  plot_bars_style   ^= BarsStacked    $
                  plot_bars_alignment ^= BarsLeft     $
                  defaultPlotBars

    plotTrackActivity :: S.ByteString -> [(t,InEvent)] -> Delta t -> ([(S.ByteString, Double)] -> Double -> Double) -> Layout1 t Double
    plotTrackActivity name es bs transform = layoutWithTitle [plotBars plot] name
      where plot = plot_bars_values      ^= barsData $
                   plot_bars_item_styles ^= itemStyles $
                   plot_bars_titles      ^= map show subTracks $
                   ourPlotBars
            itemStyles = [(solidFillStyle (opaque c), Nothing) | c <- colors]
            bins = edges2bins bs minInTime maxInTime (edges es)
            subTracks = Set.toList $ Set.fromList [s | (_,sns) <- bins, (s,n) <- sns]
            barsData = [(t, map (transform sns . fromMaybe 0 . (`lookup` sns)) subTracks) 
                       | ((t,_),sns) <- edges2bins bs minInTime maxInTime (edges es), (s,n) <- sns]

    plotTrackACount :: S.ByteString -> [(t,InEvent)] -> Delta t -> Layout1 t Double
    plotTrackACount name es bs = plotTrackActivity name es bs (\_ -> id)

    plotTrackAFreq :: S.ByteString -> [(t,InEvent)] -> Delta t -> Layout1 t Double
    plotTrackAFreq name es bs = plotTrackActivity name es bs $ \sns -> 
        let total = (\x -> if x==0 then 1 else x) $ sum [n | (s,n) <- sns] in (/total)

    plotTrackAPercent :: S.ByteString -> [(t,InEvent)] -> Delta t -> Double -> Layout1 t Double
    plotTrackAPercent name es bs b = plotTrackActivity name es bs (\_ x -> 100*x/b)

    plotTrackFreq  :: S.ByteString -> [(t,InEvent)] -> Delta t -> PlotBarsStyle -> Layout1 t Double
    plotTrackFreq  = plotTrackAtoms atoms2freqs

    plotTrackHist  :: S.ByteString -> [(t,InEvent)] -> Delta t -> PlotBarsStyle -> Layout1 t Int
    plotTrackHist  = plotTrackAtoms atoms2hist

    plotTrackAtoms :: (Num v, BarsPlotValue v) =>
                      ([S.ByteString] -> [S.ByteString] -> [v]) ->
                      S.ByteString -> [(t,InEvent)] -> Delta t -> PlotBarsStyle -> Layout1 t v
    plotTrackAtoms f name es bs k = layoutWithTitle [plotBars plot] name
      where plot = plot_bars_style       ^= k           $
                   plot_bars_values      ^= vals        $
                   plot_bars_item_styles ^= itemStyles  $
                   plot_bars_titles      ^= "":map show vs $
                   ourPlotBars
            itemStyles = none:[(solidFillStyle (opaque c), Nothing) | c <- colors]
            vals = byTimeBins ((0:).f vs) bs t0 as
            -- TODO Multiple tracks
            as   = [(t,a) | (t,_,a) <- atoms es]
            vs   = M.keys $ M.fromList $ [(a,()) | (_,a) <- as]

    -- TODO Multiple tracks
    plotTrackEvent :: S.ByteString -> [(t,InEvent)] -> Layout1 t Status
    plotTrackEvent     name es       = layoutWithTitle [toPlot plot] name
      where plot = plot_event_data           ^= dropTrack (edges2events (edges es) minInTime maxInTime) $
                   plot_event_long_fillstyle ^= toFillStyle             $
                   plot_event_label          ^= toLabel                 $
                   defaultPlotEvent
            dropTrack = map snd
            toFillStyle s = solidFillStyle . opaque $ fromMaybe lightgray (readColourName (statusColor s))
            toLabel     s = statusLabel s

    plotTrackQuantile :: S.ByteString -> [(t,InEvent)] -> [Double] -> Delta t -> Layout1 t Double
    plotTrackQuantile  name es qs bs = layoutWithTitle [plotBars plot] name
      where plot = plot_bars_values  ^= toBars (byTimeBins (getQuantiles qs) bs t0 vs) $
                   plot_bars_item_styles ^= quantileStyles $
                   plot_bars_titles  ^= quantileTitles $
                   ourPlotBars
            -- TODO Multiple tracks
            vs = [(t,v) | (t,_,v) <- values es]
            quantileStyles = none:(zip (map (solidFillStyle . opaque) colors) [Just $ solidLine 1 (opaque black) | i <- [0..n+1]])
            quantileTitles = [""]++[show p1++".."++show p2++"%" | (p1,p2) <- lag percents ]
              where
                percents = map (floor . (*100.0)) $ [0.0] ++ qs ++ [1.0]
            n = length qs

    lag :: [a] -> [(a,a)]
    lag xs = xs `zip` tail xs

    colors = cycle [green,blue,red,brown,yellow,orange,grey,purple,violet,lightblue]

    binTitles vs = [low]++[show v1++".."++show v2 | (v1,v2) <- lag vs]++[high]
      where
        low = "<"++show (head vs)
        high = ">"++show (last vs)

    binColor n i = opaque (colors !! i)

    plotTrackBinFreqs  name es vs bs = plotTrackBars vals (binTitles vs) name (binColor n)
      where
        vals = byTimeBins ((0:).values2binFreqs  vs) bs t0 tvs
        n    = length vs
        -- TODO Multiple tracks
        tvs  = [(t,v) | (t,_,v) <- values es]
    plotTrackBinHist   name es vs bs = plotTrackBars vals (binTitles vs) name (binColor n)
      where
        vals = byTimeBins ((0:).values2binHist vs) bs t0 tvs
        tvs  = [(t,v) | (t,_,v) <- values es]
        n    = length vs

    plotTrackBars :: (BarsPlotValue a) => [(t,[a])] -> [String] -> S.ByteString -> (Int -> AlphaColour Double) -> Layout1 t a
    plotTrackBars values titles name clr = layoutWithTitle [plotBars plot] name
      where plot = plot_bars_values      ^= values    $
                   plot_bars_item_styles ^= binStyles $
                   plot_bars_titles      ^= "":titles $
                   ourPlotBars
            binStyles = none:[(solidFillStyle (clr i), Just $ solidLine 1 (opaque black))
                             | (i,_) <- [0..]`zip`titles]

    none = (solidFillStyle transparent, Nothing)
    toBars tvs = [(t,diffs vs) | (t,vs) <- tvs]
    diffs xs = zipWith (-) xs (0:xs)

    groupByTrack xs = M.toList $ sort `fmap` M.fromListWith (++) [(s, [(t,v)]) | (t,s,v) <- xs]

    plotLines :: S.ByteString -> [(S.ByteString, [(t,Double)])] -> Layout1 t Double
    plotLines name vss = layoutWithTitle (map toPlot plots) name
      where plots = [plot_lines_values ^= [vs] $ 
                     plot_lines_title  ^= S.unpack subtrack $ 
                     plot_lines_style  .> line_color ^= color $ 
                     defaultPlotLines 
                     | (subtrack, vs) <- vss
                     | color <- map opaque colors]

    plotTrackLines :: S.ByteString -> [(t,InEvent)] -> Layout1 t Double
    plotTrackLines name es = plotLines name (groupByTrack (values es))

    plotTrackDots :: S.ByteString -> [(t,InEvent)] -> Layout1 t Double
    plotTrackDots  name es = layoutWithTitle (map toPlot plots) name
      where plots = [plot_points_values ^= vs $
                     plot_points_style  ^= hollowCircles 4 1 color $
                     plot_points_title  ^= S.unpack subtrack $
                     defaultPlotPoints
                     | (subtrack, vs) <- groupByTrack (values es)
                     | color <- map opaque colors]

    plotTrackCumSum :: S.ByteString -> [(t,InEvent)] -> SumSubtrackStyle -> Layout1 t Double
    plotTrackCumSum name es SumOverlayed = plotLines name rows
      where rows = [(track, scanl (\(t1,s) (t2,v) -> (t2,s+v)) (minInTime, 0) vs) | (track, vs) <- groupByTrack (values es)]
    plotTrackCumSum name es SumStacked = plotLines name rows
      where vals = values es
            allTracks = Set.toList $ Set.fromList [track | (t, track, v) <- vals]

            rows :: [(S.ByteString, [(t, Double)])]
            rows = groupByTrack [(t, track, v) | (t, tvs) <- rowsT, (track,v) <- tvs]

            rowsT :: [(t, [(S.ByteString, Double)])]
            rowsT = (minInTime, zip allTracks (repeat 0)) : St.evalState (mapM addDataPoint vals) M.empty
            
            addDataPoint (t, track, v) = do
              St.modify (M.insertWith (+) track v)
              st <- St.get
              let trackSums = map (\x -> M.findWithDefault 0 x st) allTracks
              return (t, allTracks `zip` (scanl1 (+) trackSums))


    plotTrackSum :: S.ByteString -> [(t,InEvent)] -> Delta t -> SumSubtrackStyle -> Layout1 t Double
    plotTrackSum name es bs ss = plotLines name rows
      where groups    = groupByTrack (values es)
            allTracks = M.keys $ M.fromList groups
            
            rowsT :: [(t, M.Map S.ByteString Double)]
            rowsT = byTimeBins (M.fromListWith (+)) bs t0 $ sort [(t, (track, v)) | (track, vs) <- groups, (t, v) <- vs]
            
            rowsT' = case ss of
              SumOverlayed -> map (\(t,ss) -> (t, M.toList ss)) rowsT
              SumStacked   -> map (\(t,ss) -> (t, stack ss))    rowsT

            stack :: M.Map S.ByteString Double -> [(S.ByteString, Double)]
            stack ss = zip allTracks (scanl1 (+) (map (\x -> M.findWithDefault 0 x ss) allTracks))
            
            rows :: [(S.ByteString, [(t, Double)])]
            rows  = M.toList $ sort `fmap` M.fromListWith (++) [(track, [(t,sum)]) | (t, m) <- rowsT', (track, sum) <- m]

    layoutWithTitle :: (PlotValue a) => [Plot t a] -> S.ByteString -> Layout1 t a
    layoutWithTitle plots name =
        layout1_title ^= "" $
        layout1_plots ^= map Left plots $
        layout1_bottom_axis .> laxis_generate ^= (\_ -> commonTimeAxis) $
        layout1_top_axis    .> laxis_generate ^= (\_ -> commonTimeAxis) $
        layout1_left_axis   .> laxis_title ^= S.unpack name $
        layout1_margin ^= 0 $
        layout1_grid_last ^= True $
        defaultLayout1

edges2durations :: forall t. (Ord t, HasDelta t) => [(t,S.ByteString,Edge)] -> t -> t -> S.ByteString -> [(t,InEvent)]
edges2durations tes minTime maxTime commonTrack = 
    [(t2, InValue commonTrack $ toSeconds (t2 `sub` t1) (undefined::t)) 
    | (track, LongEvent (t1,True) (t2,True) _) <- edges2events tes minTime maxTime]

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

edges2bins :: forall t. (Ord t,HasDelta t,Show t) => Delta t -> t -> t -> [(t,S.ByteString,Edge)] -> [((t,t), [(S.ByteString,Double)])]
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
      RWS.tell [(bin, [(s, (fromIntegral npulse/binSizeSec + area + toSeconds (t2 `sub` start) t2*nopen)/binSizeSec) | (s,(area,start,nopen,npulse)) <- states])]
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
    step' (t, s, Pulse _) = modState s t id
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
    toFreq = if n==0 then const 0 else (\k -> fromIntegral k/fromIntegral n)
values2binHist bins xs = values2binHist' bins $ sort xs
  where
    values2binHist' []     xs = [length xs]
    values2binHist' (a:as) xs = length xs0 : values2binHist' as xs'
      where (xs0,xs') = span (<a) xs

atoms2hist :: (Ord a) => [a] -> [a] -> [Int]
atoms2hist as xs = map (maybe 0 id . (`M.lookup` m)) as
  where
    m          = foldl' insert M.empty xs
    insert m a = M.alter (Just . maybe 1 inc) a m
    inc    n   = n `seq` (n+1)

atoms2freqs :: (Ord a) => [a] -> [a] -> [Double]
atoms2freqs as xs = map toFreq (atoms2hist as xs)
  where
    n = length xs
    toFreq = if n==0 then const 0 else (\k -> fromIntegral k/fromIntegral n)

zoom :: (TimeAxis t) => [(t, InEvent)] -> Maybe t -> Maybe t -> [(t, InEvent)]
zoom events fromTime toTime = filter p events
  where
    p (t, _) = (maybe True (\ft -> t >= ft) fromTime) &&
               (maybe True (\tt -> t <  tt) toTime)

showHelp = mapM_ putStrLn [ "",
  "tplot - a tool for drawing timing diagrams.",
  "        See http://www.haskell.org/haskellwiki/Timeplot",
  "Usage: tplot [-o OFILE] [-of {png|pdf|ps|svg|x}] [-or 640x480]",
  "             -if IFILE [-tf TF] ",
  "             [{+|-}k Pat1 Kind1 {+|-}k Pat2 Kind2 ...] [{+|-}dk KindN]",
  "             [-fromTime TIME] [-toTime TIME] [-baseTime TIME]",
  "  -o  OFILE  - output file (required if -of is not x)",
  "  -of        - output format (x means draw result in a window, default:",
  "               extension of -o)",
  "  -or        - output resolution (default 640x480)",
  "  -if IFILE  - input file; '-' means 'read from stdin'",
  "  -tf TF     - time format: 'num' means that times are floating-point",
  "               numbers (for instance, seconds elapsed since an event);",
  "               'date PATTERN' means that times are dates in the format",
  "               specified by PATTERN - see",
  "               http://linux.die.net/man/3/strptime, for example,",
  "               [%Y-%m-%d %H:%M:%S] parses dates like [2009-10-20 16:52:43].",
  "               We also support %OS for fractional seconds (i.e. %OS will",
  "               parse 12.4039 or 12,4039) and %^[+-][N]s for ten-powers ",
  "               of seconds since epoch, for example %^-3s is ms since epoch.",
  "               Default: 'date %Y-%m-%d %H:%M:%OS'",
  "  {+|-}dk    - set default diagram kind",
  "  {+|-}k P K - set diagram kind for tracks matching regex P (in the format",
  "               of regex-tdfa, which is at least POSIX-compliant and",
  "               supports some GNU extensions) to K",
  "               EXPLANATION:",
  "               -k clauses are matched till first success, +k are all",
  "               matched: a track is drawn acc. to all matching +k, to +dk",
  "               AND ALSO to the first matching -k, or -dk if none of -k",
  "               match",
  "  -fromTime  - filter records whose time is >= this time",
  "               (formatted according to -tf)",
  "  -toTime    - filter records whose time is <  this time",
  "               (formatted according to -tf)",
  "  -baseTime  - display time difference with this value instead of absolute time",
  "               (formatted according to -tf)",
  "  -zoomMode M- whether -fromTime and -toTime filter the input or the output:",
  "               zooming input (dropping events outside interval) may be faster,",
  "               but zooming output will be more accurate for history-dependent",
  "               graphs like activity graphs. M is 'input' or 'output'.",
  "",
  "Input format: lines of the following form:",
  "1234 >A - at time 1234, activity A has begun",
  "1234 <A - at time 1234, activity A has ended",
  "1234 !B - at time 1234, pulse event B has occured",
  "1234 !B TEXT - at time 1234, pulse event B has occured with label TEXT",
  "1234 @B COLOR - at time 1234, the status of B became such that it is",
  "                appropriate to draw it with color COLOR :)",
  "1234 =C VAL - at time 1234, parameter C had numeric value VAL (for example,",
  "              HTTP response time)",
  "1234 =D `EVENT - at time 1234, event EVENT occured in process D (for",
  "                 example, HTTP response code)",
  "It is assumed that many events of the same kind may occur at once.",
  "Diagram kinds:",
  "  'none' - do not plot this track",
  "  'event' is for event diagrams: activities are drawn like --[===]--- ,",
  "     pulse events like --|-- with a label over '|'",
  "  'duration XXXX' - plot any kind of diagram over the *durations* of events",
  "     on a track (delimited by > ... <), for example 'duration quantile",
  "     300 0.25,0.5,0.75' will plot these quantiles of durations of the",
  "     events. This is useful where your log looks like 'Started processing'",
  "     ... 'Finished processing': you can plot processing durations without",
  "     computing them yourself. Very useful inside 'within'!",
  "  'within[C] XXXX' - draw plot XXXX over events grouped by their track's name ",
  "     before separator C. For example, if you have processes",
  "     named 'MACHINE-PID' (i.e. UNIT027-8532) say 'begin something' / ",
  "     'end something' and you're interested in the properties of per-machine",
  "     durations, use within[-] duration dots; or if you've got jobs starting",
  "     and finishing tasks on different machines, and you want to plot a diagram",
  "     showing the number of utilized machines and how this number is composed of",
  "     utilization by different jobs, make your trace say '>job-JOBID'...'<job-JOBID'",
  "     and use -k job 'within[-] count 1'.",
  "     Explanation: if you specify -k REGEX 'within[.] SOMETHING', timeplot will",
  "     take all tracks matching REGEX, split each track around the first '.', giving",
  "     a 'supertrack' and 'subtrack' (e.g. customer.John -> customer, John), ",
  "     group the events by supertrack and for each supertrack draw a graphical track",
  "     using the plot type SOMETHING. It's up to SOMETHING to do something with these",
  "     events, e.g. 'lines' will simply draw several line plots, one per subtrack.",
  "  'acount N' is for activity counts: a histogram is drawn with granularity",
  "     of N time units, where the bin corresponding to [t..t+N) has value",
  "     'what was the average number of active events or impulses in that",
  "     interval'. When used inside 'within', the histogram is a stacked one,",
  "     with one vertical bar per subtrack in each bin.",
  "  'apercent N B' is for activity percentages of a basis: like 'acount N',",
  "     but instead of X you get 100*X/B",
  "  'afreq N' is for activity frequencies: it's like acount, but relative",
  "     rather than absolute - it only makes sense inside 'within', because",
  "     otherwise it would just always show a filled one-coloured bar in every bin.",
  "  'freq N [TYPE]' is for event frequency histograms: a histogram of type",
  "     TYPE (stacked or clustered, default clustered) is drawn for each time",
  "     bin of size N, about the *frequency* of various ` events",
  "  'hist N [TYPE]' is for event count histograms: a histogram of type TYPE",
  "     (stacked or clustered, default clustered) is drawn for each time bin",
  "     of size N, about the *counts* of various ` events",
  "  'quantile N q1,q2,..' (example: quantile 100 0.25,0.5,0.75) - a bar chart",
  "     of corresponding quantiles in time bins of size N",
  "  'binf N v1,v2,..' (example: binf 100 1,2,5,10) - a histogram of frequency",
  "     of values falling into bins min..v1, v1..v2, .., v2..max in time bins",
  "     of size N",
  "  'binh N v1,v2,..' (example: binf 100 1,2,5,10) - a histogram of counts of",
  "     values falling into bins min..v1, v1..v2, .., v2..max in time bins of",
  "     size N",
  "  'lines'  - a simple line plot of numeric values. When used in 'within', ",
  "     gives one plot per subtrack.",
  "  'dots'   - a simple dot plot of numeric values. When used in 'within', ",
  "     gives one plot per subtrack.",
  "  'cumsum [TYPE]' - a simple line plot of the sum of the numeric values.",
  "     When used in 'within', produce 1 subplot per subtrack. TYPE can be: ",
  "     'overlayed' -> just lay the subplots over one another.",
  "     'stacked'   -> add them up at each point to see how subtracks contribute",
  "     to the total cumulative sum (default; only makes sense inside 'within')",
  "  'sum N [TYPE]' - a simple line plot of the sum of the numeric values in time",
  "     bins of size N. N is measured in units or in seconds.",
  "     When used in 'within', produce 1 subplot per subtrack. TYPE used in same ",
  "     way as in cumsum."
  ]


main = do
  args <- getArgs
  mainWithArgs args
mainWithArgs args = do
  when (null args || args == ["--help"]) $ showHelp >> exitSuccess
  case (readConf args) of
    Conf conf -> do
      let render = case (outFormat conf) of {
          PNG    -> \c w h f -> const () `fmap` renderableToPNGFile c w h f;
          PDF    -> renderableToPDFFile ;
          PS     -> renderableToPSFile  ;
          SVG    -> renderableToSVGFile ;
          Window -> \c w h f -> renderableToWindow c w h
        }
      case conf of
        ConcreteConf {
            parseTime=parseTime, inFile=inFile, chartKindF=chartKindF,
            outFile=outFile, outResolution=outResolution,
            fromTime=fromTime, toTime=toTime, transformLabel=transformLabel, zoomMode=zoomMode} -> do
          source <- readSource parseTime inFile
          let source' = case zoomMode of {
              ZoomInput  -> zoom source fromTime toTime ;
              ZoomOutput -> source
            }
          let chart = makeChart chartKindF source' fromTime toTime zoomMode transformLabel
          let (w,h) = outResolution
          render chart w h outFile
