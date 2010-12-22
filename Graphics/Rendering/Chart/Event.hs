{-# LANGUAGE TemplateHaskell #-}
module Graphics.Rendering.Chart.Event (
    PlotEvent(..),
    Event(..),

    defaultPlotEvent,
    plot_event_title,
    plot_event_data,
    plot_event_long_fillstyle,
    plot_event_long_linestyle,
    plot_event_pulse_linestyle,
    plot_event_track_linestyle,
    plot_event_label,
) where

import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Accessor
import Data.Accessor.Template
import Control.Monad

data Event t e = LongEvent t t e  -- ^ An event that has a beginning and an end
               | PulseEvent t e   -- ^ A zero-length event
               deriving (Show)

-- | A chart that depict events.
-- There are two kinds of events: long and impulse events. A long event
-- is drawn like "--[=====]---" and has a beginning and ending moment, and
-- an impulse event is drawn like "---|----" and has an occurence moment.
data PlotEvent t e = PlotEvent {
   plot_event_title_           :: String,
   plot_event_data_            :: [Event t e],
   -- | Linestyle with which marks for pulse events will be drawn
   plot_event_pulse_linestyle_ :: e -> CairoLineStyle,
   -- | Linestyle with which borders of rectangles for long events will be drawn
   plot_event_long_linestyle_  :: e -> CairoLineStyle,
   -- | Fillstyle with which interiors of rectangles for long events will be filled
   plot_event_long_fillstyle_  :: e -> CairoFillStyle,
   -- | Linestyle with which the "track line" will be drawn
   plot_event_track_linestyle_ :: CairoLineStyle,
   plot_event_label_           :: e -> String
}

defaultPlotEvent = PlotEvent {
   plot_event_title_           = "",
   plot_event_data_            = [],
   plot_event_pulse_linestyle_ = const $ solidLine 2 (opaque red),
   plot_event_long_linestyle_  = const $ solidLine 1 (opaque black),
   plot_event_long_fillstyle_  = const $ solidFillStyle (opaque lightgray),
   plot_event_track_linestyle_ = solidLine 1 (opaque black),
   plot_event_label_           = const ""
}

instance ToPlot PlotEvent where
    toPlot p = Plot {
        plot_render_ = renderPlotEvent p,
	    plot_legend_ = [(plot_event_title_ p, renderPlotLegendEvent p)],
	    plot_all_points_ = plotAllPointsEvent p
    }

renderPlotLegendEvent :: PlotEvent t e -> Rect -> CRender ()
renderPlotLegendEvent p r = return ()


filledRect :: CairoFillStyle -> Rect -> CRender ()
filledRect fs r = setFillStyle fs >> fillPath (rectPath r)

framedRect :: CairoLineStyle -> Rect -> CRender ()
framedRect ls r = setLineStyle ls >> strokePath (rectPath r)

barHeight = 7
pulseHeight = 15

renderPlotEvent :: PlotEvent t e -> PointMapFn t e  -> CRender ()
renderPlotEvent p pmap = do
      setLineStyle $ plot_event_track_linestyle_ p
      moveTo (Point x0 cy)
      lineTo (Point x1 cy)
      c $ C.stroke
      mapM_ drawEventFill  (plot_event_data_ p)
      mapM_ drawEventFrame (plot_event_data_ p)
    where
      (Point x0 y0) = pmap (LMin,LMin)
      (Point x1 y1) = pmap (LMax,LMax)
      (cx,cy) = ((x0+x1)/2, (y0+y1)/2)
      drawEventFill (PulseEvent t e) = return ()
      drawEventFill (LongEvent t1 t2 e) = do
        let (Point x1 cy)  = pmap (LValue t1, LValue e)
        let (Point x2 cy') = pmap (LValue t2, LValue e) -- Assume cy' == cy (pmap is coordinate-wise)
        filledRect (plot_event_long_fillstyle_ p e) $ Rect
            (Point x1 (cy-barHeight/2)) (Point x2 (cy+barHeight/2))

      drawEventFrame (PulseEvent t e) = do
        setLineStyle $ plot_event_pulse_linestyle_ p e
        let (Point x y) = pmap (LValue t, LValue e)
        moveTo (Point x (y-pulseHeight/2))
        lineTo (Point x (y+pulseHeight/2))
        c $ C.stroke
        let label = plot_event_label_ p e
        when (not (null label)) $ do
          extents <- c $ C.textExtents label
          moveTo (Point x (y - pulseHeight/2 - C.textExtentsHeight extents - C.textExtentsYbearing extents - 1))
          setLineStyle $ solidLine 2 (opaque black)
          c $ C.showText label
      drawEventFrame (LongEvent t1 t2 e) = do
        let (Point x1 cy)  = pmap (LValue t1, LValue e)
        let (Point x2 cy') = pmap (LValue t2, LValue e) -- Assume cy' == cy (pmap is coordinate-wise)
        framedRect (plot_event_long_linestyle_ p e) $ Rect
            (Point x1 (cy-barHeight/2)) (Point x2 (cy+barHeight/2))

plotAllPointsEvent :: PlotEvent t e -> ([t], [e])
plotAllPointsEvent p = let (ts, es) = unzip (map decomp d) in (concat ts, es)
  where
    d = plot_event_data_ p
    decomp (PulseEvent t     e) = ([t],     e)
    decomp (LongEvent  t1 t2 e) = ([t1,t2], e)

$( deriveAccessors ''PlotEvent )
