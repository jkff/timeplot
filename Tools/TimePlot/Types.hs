{-# LANGUAGE CPP, TypeFamilies #-}
module Tools.TimePlot.Types where

import Data.Time hiding (parseTime)
import qualified Data.ByteString.Char8 as S
import Graphics.Rendering.Chart
import Data.Colour
import Data.Accessor
import Graphics.Rendering.Chart.Event

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
#ifdef HAVE_GTK
               | Window
#endif

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

instance HasDelta UTCTime where
  type Delta UTCTime = NominalDiffTime
  add d t = addUTCTime d t
  sub t2 t1 = diffUTCTime t2 t1
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
  readsPrec n s = [(fromSeconds i (undefined::UTCTime), s') | (i,s') <- readsPrec n s]

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
               | KindDots      { alpha :: Double }
               | KindCumSum    { subtrackStyle :: SumSubtrackStyle }
               | KindSum       { binSize :: Delta t, subtrackStyle :: SumSubtrackStyle }
               | KindNone
               | KindUnspecified -- Causes an error message 

instance Show CairoLineStyle where show _ = "<line>"
instance Show CairoFillStyle where show _ = "<fill>"
data PlotData = PlotBarsData
                {
                    plotName :: String,
                    barsStyle :: PlotBarsStyle, 
                    barsValues :: [ (UTCTime, [Double]) ], 
                    barsStyles :: [(CairoFillStyle, Maybe CairoLineStyle)], 
                    barsTitles :: [String] 
                }
              | PlotEventData
                {
                    plotName :: String,
                    eventData :: [Event UTCTime Status]
                }
              | PlotLinesData
                {
                    plotName :: String,
                    linesData :: [[(UTCTime, Double)]],
                    linesStyles :: [CairoLineStyle],
                    linesTitles :: [String]
                }
              | PlotDotsData
                {
                    plotName :: String,
                    dotsData :: [[(UTCTime, Double)]],
                    dotsColors :: [AlphaColour Double],
                    dotsTitles :: [String]
                }
              deriving (Show)
