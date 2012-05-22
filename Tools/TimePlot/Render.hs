{-# LANGUAGE ParallelListComp #-}
module Tools.TimePlot.Render (
    dataToPlot
) where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Event
import Data.Time
import Data.Accessor
import Data.Colour
import Data.Colour.Names
import Data.Maybe

import Tools.TimePlot.Types
import Tools.TimePlot.Plots

mapAxisData :: (a -> b) -> (b -> a) -> AxisData b -> AxisData a
mapAxisData f f' (AxisData vp pv ticks labels grid) = AxisData 
    (\range x -> vp range (f x)) 
    (\range v -> f' (pv range v)) 
    (map (\(x,t) -> (f' x, t)) ticks) 
    (map (map (\(x,lab) -> (f' x, lab))) labels) 
    (map f' grid)

dataToPlot :: AxisData LocalTime -> PlotData -> AnyLayout1 LocalTime
dataToPlot commonTimeAxis p@PlotBarsData{} = withAnyOrdinate $ layoutWithTitle commonTimeAxis [plotBars plot] (plotName p) (length (barsTitles p) > 1)
  where plot = plot_bars_values      ^= barsValues p $
               plot_bars_item_styles ^= barsStyles p $
               plot_bars_style       ^= barsStyle p $
               plot_bars_titles      ^= barsTitles p $
               ourPlotBars
dataToPlot commonTimeAxis p@PlotEventData{} = withAnyOrdinate $ layoutWithTitle commonTimeAxis [toPlot plot] (plotName p) False
  where plot = plot_event_data           ^= eventData p $
               plot_event_long_fillstyle ^= toFillStyle $
               plot_event_label          ^= toLabel     $
               defaultPlotEvent
        toFillStyle s = solidFillStyle . opaque $ fromMaybe lightgray (readColourName (statusColor s))
        toLabel     s = statusLabel s 
dataToPlot commonTimeAxis p@PlotLinesData{} = withAnyOrdinate $ layoutWithTitle commonTimeAxis (map toPlot plots) (plotName p) (length (linesData p) > 1)
  where plots = [plot_lines_values ^= [vs] $ 
                 plot_lines_title  ^= title $ 
                 plot_lines_style  ^= lineStyle $ 
                 defaultPlotLines 
                 | vs <- linesData p
                 | title <- linesTitles p
                 | lineStyle <- linesStyles p]
dataToPlot commonTimeAxis p@PlotDotsData{} = withAnyOrdinate $ layoutWithTitle commonTimeAxis (map toPlot plots) (plotName p) (length (dotsData p) > 1)
  where plots = [plot_points_values ^= vs $
                 plot_points_style  ^= hollowCircles 4 1 color $
                 plot_points_title  ^= subtrack $
                 defaultPlotPoints
                 | subtrack <- dotsTitles p
                 | color <- dotsColors p
                 | vs <- dotsData p]

layoutWithTitle :: (PlotValue a) => AxisData LocalTime -> [Plot LocalTime a] -> String -> Bool -> Layout1 LocalTime a
layoutWithTitle commonTimeAxis plots name showLegend =
    layout1_title ^= "" $
    layout1_plots ^= map Left plots $
    (if showLegend then id else (layout1_legend ^= Nothing)) $
    layout1_bottom_axis .> laxis_generate ^= (\_ -> commonTimeAxis) $
    layout1_top_axis    .> laxis_generate ^= (\_ -> commonTimeAxis) $
    layout1_left_axis   .> laxis_title ^= name $
    layout1_margin ^= 0 $
    layout1_grid_last ^= True $
    defaultLayout1

ourPlotBars :: (BarsPlotValue a) => PlotBars LocalTime a
ourPlotBars = plot_bars_spacing ^= BarsFixGap 0 0 $
              plot_bars_style   ^= BarsStacked    $
              plot_bars_alignment ^= BarsLeft     $
              defaultPlotBars
