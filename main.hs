module Main where

import Harriss
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events
import Data.IORef

main = do
  initGUI
  win <- windowNew
  onDestroy win mainQuit
  windowSetDefaultSize win 300 300

  vBox <- vBoxNew False 0
  containerAdd win vBox

  guiState <- newIORef $ GUIState 0 True False False False

  plot <- drawingAreaNew
  boxPackStart vBox plot PackGrow 0
  onExpose plot $ drawPlot plot guiState

  --need to force repaint in signal handlers
  gen <- spinButtonNewWithRange 0.0 100000.0 1.0
  boxPackStart vBox gen PackNatural 1
  onValueSpinned gen (updateGeneration gen guiState)
  
  -- should probably be using lenses below
  drawCurves <- checkButtonNewWithMnemonic "Draw _Curves"
  boxPackStart vBox drawCurves PackNatural 1
  toggleButtonSetActive drawCurves True
  on drawCurves buttonActivated (updateCheckedState drawCurves guiState (\c s -> s {drawCurves = c}))

  drawSquares <- checkButtonNewWithMnemonic "Draw _Squares"
  boxPackStart vBox drawSquares PackNatural 1
  on drawSquares buttonActivated (updateCheckedState drawSquares guiState (\c s -> s {drawSquares = c}))
 
  colorSquares <- checkButtonNewWithMnemonic "Color _Squares"
  boxPackStart vBox colorSquares PackNatural 1
  on colorSquares buttonActivated (updateCheckedState colorSquares guiState (\c s -> s {colorSquares = c}))

  drawSplitRects <- checkButtonNewWithMnemonic "Draw _Split Rectangles"
  boxPackStart vBox drawSplitRects PackNatural 1
  on drawSplitRects buttonActivated (updateCheckedState drawSplitRects guiState (\c s -> s {drawSplitRects = c}))

  widgetShowAll win
  
  mainGUI

data GUIState = GUIState {numGens::Int, drawCurves::Bool, drawSquares::Bool, colorSquares::Bool, drawSplitRects::Bool}
              deriving Show

drawPlot :: DrawingArea -> IORef GUIState -> Event -> IO Bool
drawPlot plot guiState _ = do
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

updateGeneration :: SpinButton -> IORef GUIState -> IO ()
updateGeneration sb state = do
  g  <- spinButtonGetValue sb
  print g
  modifyIORef state (\s -> s {numGens = round g})
  readIORef state >>= print
  

updateCheckedState :: CheckButton -> IORef GUIState -> (Bool -> GUIState -> GUIState) -> IO ()
updateCheckedState button state update = do
    checked <- toggleButtonGetActive button 
    modifyIORef state (update checked)
