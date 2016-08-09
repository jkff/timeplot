{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Graphics.Rendering.Chart.Event (
    PlotEvent(..),
    Event(..),

    eventStart,
    eventEnd,

    plot_event_title,
    plot_event_data,
    plot_event_long_fillstyle,
    plot_event_long_linestyle,
    plot_event_pulse_linestyle,
    plot_event_track_linestyle,
    plot_event_label,
) where

import Control.Lens
import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default
import Control.Monad

#if ! MIN_VERSION_Chart(1,7,0)
-- legacy map for Chart<1.7
#    define BackendProgram ChartBackend
#endif

data Event t e = LongEvent (t,Bool) (t,Bool) e  -- ^ An event that has a beginning and an end. 
                                                --   True = "known explicitly", False = "implicit" (e.g. imposed by axis bounds)
               | PulseEvent t e   -- ^ A zero-length event
               deriving (Show)

eventStart :: Event t e -> t
eventStart (LongEvent (t0,_) (_,_) _) = t0
eventStart (PulseEvent t _) = t

eventEnd :: Event t e -> t
eventEnd (LongEvent (_,_) (t1,_) _) = t1
eventEnd (PulseEvent t _) = t

-- | A chart that depict events.
-- There are two kinds of events: long and impulse events. A long event
-- is drawn like "--[=====]---" and has a beginning and ending moment, and
-- an impulse event is drawn like "---|----" and has an occurence moment.
data PlotEvent t e = PlotEvent {
   _plot_event_title           :: String,
   _plot_event_data            :: [Event t e],
   -- | Linestyle with which marks for pulse events will be drawn
   _plot_event_pulse_linestyle :: e -> LineStyle,
   -- | Linestyle with which borders of rectangles for long events will be drawn
   _plot_event_long_linestyle  :: e -> LineStyle,
   -- | Fillstyle with which interiors of rectangles for long events will be filled
   _plot_event_long_fillstyle  :: e -> FillStyle,
   -- | Linestyle with which the "track line" will be drawn
   _plot_event_track_linestyle :: LineStyle,
   _plot_event_label           :: e -> String
}
makeLenses ''PlotEvent

instance Default (PlotEvent t e) where
  def = PlotEvent {
    _plot_event_title           = "",
    _plot_event_data            = [],
    _plot_event_pulse_linestyle = const $ solidLine 2 (opaque red),
    _plot_event_long_linestyle  = const $ solidLine 1 (opaque black),
    _plot_event_long_fillstyle  = const $ solidFillStyle (opaque lightgray),
    _plot_event_track_linestyle = solidLine 1 (opaque black),
    _plot_event_label           = const ""
  }

instance ToPlot PlotEvent where
    toPlot p = Plot {
      _plot_render = renderPlotEvent p,
      _plot_legend = [(_plot_event_title p, renderPlotLegendEvent p)],
      _plot_all_points = plotAllPointsEvent p
    }

renderPlotLegendEvent :: PlotEvent t e -> Rect -> BackendProgram ()
renderPlotLegendEvent p r = return ()


filledRect :: FillStyle -> Rect -> BackendProgram ()
filledRect fs r = withFillStyle fs $ fillPath (rectPath r)

framedRect :: LineStyle -> Rect -> BackendProgram ()
framedRect ls r = withLineStyle ls $ strokePath (rectPath r)

barHeight = 7
pulseHeight = 15

renderPlotEvent :: PlotEvent t e -> PointMapFn t e  -> BackendProgram ()
renderPlotEvent p pmap = do
      withLineStyle (p ^. plot_event_track_linestyle) $ do
        strokePointPath [Point x0 cy, Point x1 cy]
        mapM_ drawEventFill  (p ^. plot_event_data)
        mapM_ drawEventFrame (p ^. plot_event_data)
    where
      (Point x0 y0) = pmap (LMin,LMin)
      (Point x1 y1) = pmap (LMax,LMax)
      (cx,cy) = ((x0+x1)/2, (y0+y1)/2)
      
      drawEventFill (PulseEvent t e) = return ()
      drawEventFill (LongEvent (t1,_) (t2,_) e) = do
        let (Point x1 cy)  = pmap (LValue t1, LValue e)
        let (Point x2 cy') = pmap (LValue t2, LValue e) -- Assume cy' == cy (pmap is coordinate-wise)
        filledRect (p ^. plot_event_long_fillstyle $ e) $ Rect
            (Point x1 (cy-barHeight/2)) (Point x2 (cy+barHeight/2))

      drawEventFrame (PulseEvent t e) = do
        withLineStyle (p ^. plot_event_pulse_linestyle $ e) $ do
          let (Point x y) = pmap (LValue t, LValue e)
          strokePointPath [Point x (y-pulseHeight/2), Point x (y+pulseHeight/2)]
          let label = p ^. plot_event_label $ e
          unless (null label) $ do
            textSize <- textSize label
            withLineStyle (solidLine 2 $ opaque black) $ do
              drawText (Point x (y - pulseHeight/2 - textSizeHeight textSize - textSizeYBearing textSize - 1)) label
      drawEventFrame (LongEvent (t1,_) (t2,_) e) = do
        let (Point x1 cy)  = pmap (LValue t1, LValue e)
        let (Point x2 cy') = pmap (LValue t2, LValue e) -- Assume cy' == cy (pmap is coordinate-wise)
        framedRect (p ^. plot_event_long_linestyle $ e) $ Rect
            (Point x1 (cy-barHeight/2)) (Point x2 (cy+barHeight/2))

plotAllPointsEvent :: PlotEvent t e -> ([t], [e])
plotAllPointsEvent p = (concat ts, es)
  where
    decomp (PulseEvent t             e) = ([t],     e)
    decomp (LongEvent  (t1,_) (t2,_) e) = ([t1,t2], e)
    (ts, es) = unzip $ p ^.. plot_event_data . traverse . to decomp
