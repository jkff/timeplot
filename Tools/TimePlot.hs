{-# LANGUAGE ScopedTypeVariables, TypeFamilies, ParallelListComp, CPP #-}
module Main where

import Control.Monad
import Data.List
import Data.Ord
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S

import System.Environment
import System.Exit

import Data.Time hiding (parseTime)

import Data.Accessor

import Graphics.Rendering.Chart

#ifdef HAVE_GTK
import Graphics.Rendering.Chart.Gtk
#endif

import Tools.TimePlot.Types
import Tools.TimePlot.Conf
import Tools.TimePlot.Source
import Tools.TimePlot.Plots
import Tools.TimePlot.Render

makeChart :: (S.ByteString -> [ChartKind LocalTime]) -> 
             [(LocalTime, InEvent)] ->
             Maybe LocalTime -> Maybe LocalTime ->
             ZoomMode -> 
             (LocalTime -> String -> String) -> 
             Renderable ()
makeChart chartKindF []      minT maxT zoomMode transformLabel = emptyRenderable
makeChart chartKindF events0 minT maxT zoomMode transformLabel = renderLayout1sStacked $ 
    map (dataToPlot commonTimeAxis) $ 
    makePlots chartKindF events0 minInTime maxInTime zoomMode transformLabel
  where
    times             :: [LocalTime]
    times             = sort $ [t | (t,_)<- events0]

    minInTime  = case (zoomMode, minT) of (ZoomInput,  Just t) -> t ; _ -> head times
    maxInTime  = case (zoomMode, maxT) of (ZoomInput,  Just t) -> t ; _ -> last times

    minOutTime = case (zoomMode, minT) of (ZoomOutput, Just t) -> t ; _ -> head times
    maxOutTime = case (zoomMode, maxT) of (ZoomOutput, Just t) -> t ; _ -> last times

    commonTimeAxis :: AxisData LocalTime
    commonTimeAxis = transformLabels $ autoAxis axisTimes
      where
        axisTimes = case zoomMode of
          ZoomInput  -> [minInTime, maxInTime]
          ZoomOutput -> [minOutTime, maxOutTime]
        transformLabels axis = axis { axis_labels_ = map (map (\(t, s) -> (t, transformLabel t s))) (axis_labels_ axis) }

makePlots :: (S.ByteString -> [ChartKind LocalTime]) -> 
             [(LocalTime, InEvent)] ->
             LocalTime -> LocalTime ->
             ZoomMode -> 
             (LocalTime -> String -> String) -> 
             [PlotData]
makePlots chartKindF events0 minInTime maxInTime zoomMode transformLabel = plots
  where
    events :: [(LocalTime, InEvent)]
    events = sortBy (comparing (\(t,_)-> t)) events0

    track2events :: M.Map S.ByteString [(LocalTime, InEvent)]
    track2events = reverse `fmap` foldl' insert M.empty [(evt_track e, x) | x@(t, e) <- events]
      where insert m (s, r) = M.alter (Just . maybe [r] (r:)) s m

    plots          = [ plotTrack (S.unpack track) kind es minInTime maxInTime 
                     | (track, es) <- M.toList track2events,
                       kind <- chartKindF track,
                       case kind of {KindNone -> False ; KindWithin _ _ -> False ; _ -> True} ] ++
                     withinPlots

    withinPlots  = [ plotTrack (S.unpack name) k es minInTime maxInTime | (name, (k,es)) <- M.toList withinTracks ]
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

zoom :: (Ord t) => [(t, InEvent)] -> Maybe t -> Maybe t -> [(t, InEvent)]
zoom events fromTime toTime = filter p events
  where
    p (t, _) = (maybe True (\ft -> t >= ft) fromTime) &&
               (maybe True (\tt -> t <  tt) toTime)

showHelp = mapM_ putStrLn [ "",
  "tplot - a tool for drawing timing diagrams.",
  "        See http://www.haskell.org/haskellwiki/Timeplot",
#ifdef HAVE_GTK  
  "Usage: tplot [-o OFILE] [-of {png|pdf|ps|svg|x}] [-or 640x480]",
  "             -if IFILE [-tf TF] ",
  "             [{+|-}k Pat1 Kind1 {+|-}k Pat2 Kind2 ...] [{+|-}dk KindN]",
  "             [-fromTime TIME] [-toTime TIME] [-baseTime TIME]",
  "  -o  OFILE  - output file (required if -of is not x)",
  "  -of        - output format (x means draw result in a window, default:",
  "               extension of -o)",
#else
  "Usage: tplot [-o OFILE] [-of {png|pdf|ps|svg}] [-or 640x480]",
  "             -if IFILE [-tf TF] ",
  "             [{+|-}k Pat1 Kind1 {+|-}k Pat2 Kind2 ...] [{+|-}dk KindN]",
  "             [-fromTime TIME] [-toTime TIME] [-baseTime TIME]",
  "  -o  OFILE  - output file",
  "  -of        - output format (default: extension of -o)",
#endif
  "  -or        - output resolution (default 640x480)",
  "  -if IFILE  - input file; '-' means 'read from stdin'",
  "  -tf TF     - time format: -tf 'date PATTERN' means that times are dates in the format",
  "               specified by PATTERN - see http://linux.die.net/man/3/strptime, ",
  "               for example, -tf 'date [%Y-%m-%d %H:%M:%S]' parses dates like ",
  "               '[2009-10-20 16:52:43]'.",
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
  "  'dots ALPHA' - a simple dot plot of numeric values. When used in 'within', ",
  "     gives one plot per subtrack. All dots are drawn with opacity ALPHA,",
  "     where 0 means transparent and 1 means opaque. Useful when you're suffering",
  "     from overplotting (dots overlapping each other too much)",
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
  let !conf = readConf args
  let render = case (outFormat conf) of {
      PNG    -> \c w h f -> const () `fmap` renderableToPNGFile c w h f;
      PDF    -> renderableToPDFFile ;
      PS     -> renderableToPSFile  ;
      SVG    -> renderableToSVGFile ;
#ifdef HAVE_GTK          
      Window -> \c w h f -> renderableToWindow c w h
#endif          
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
