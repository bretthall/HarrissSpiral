module Main where

import Harriss
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events

main = do
  initGUI
  win <- windowNew
  onDestroy win mainQuit
  windowSetDefaultSize win 300 300

  vBox <- vBoxNew False 0
  containerAdd win vBox

  plot <- drawingAreaNew
  boxPackStart vBox plot PackGrow 0
  onExpose plot $ clearPlot plot
               
  drawSquares <- checkButtonNewWithMnemonic "Draw _Squares"
  boxPackStart vBox drawSquares PackNatural 3

  widgetShowAll win
  
  mainGUI

clearPlot :: DrawingArea -> Event -> IO Bool
clearPlot plot _ = do
  win <- widgetGetDrawWindow plot
  (w, h) <- widgetGetSize plot
  renderWithDrawable win $ do
                     p0 <- deviceToUser 0.0 0.0 
                     liftIO $ putStrLn $ "(0,0) = " ++ show p0
                     (uw, uh) <- deviceToUser (fromIntegral w) (fromIntegral h) 
                     liftIO $ putStrLn $ "(w,h) = " ++ show (uw, uh)

                     translate 0.0 uh
                     if w < h
                     then scale (uw/1000.0)(-uw/1000.0)
                     else scale (uh/1000.0)(-uh/1000.0)

                     setSourceRGB 1.0 1.0 1.0
                     paint

                     setSourceRGB 0.0 0.0 0.0
                     moveTo 0.0 0.0
                     lineTo 500.0 500.0
                     stroke

                     newPath
                     setSourceRGB 0.0 1.0 0.0
                     moveTo 500.0 500.0
                     lineTo 1000.0 1000.0
                     stroke

  return True