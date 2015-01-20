{-# LANGUAGE  TemplateHaskell, Rank2Types#-}

module Main where

import Harriss
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events
import Data.IORef
import Control.Lens

data GUIState = GUIState {_numGens::Int, _drawCurves::Bool, _drawSquares::Bool, _colorSquares::Bool, _drawSplitRects::Bool}
              deriving Show

$(makeLenses ''GUIState)

makeCheckButton :: String -> Lens' GUIState Bool -> IORef GUIState -> VBox -> IO CheckButton
makeCheckButton text lens stateRef box = do
  button <- checkButtonNewWithMnemonic text
  boxPackStart box button PackNatural 1
  state <- readIORef stateRef
  toggleButtonSetActive button $ state^.lens
  on button buttonActivated (updateCheckedState button stateRef lens)
  return button
    where
      updateCheckedState :: CheckButton -> IORef GUIState -> Lens' GUIState Bool -> IO ()
      updateCheckedState button state lens = do
                                           checked <- toggleButtonGetActive button 
                                           modifyIORef state $ lens.~checked
                                           readIORef state >>= print
                                                                   
updateGeneration :: SpinButton -> IORef GUIState -> IO ()
updateGeneration sb state = do
  g  <- spinButtonGetValue sb
  modifyIORef state $ numGens.~(round g)
  readIORef state >>= print

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
  
  makeCheckButton "Draw _Curves" drawCurves guiState vBox
  makeCheckButton "Draw _Squares" drawSquares guiState vBox
  makeCheckButton "Color _Squares" colorSquares guiState vBox
  makeCheckButton "Draw _Split Rectangles" drawSplitRects guiState vBox

  widgetShowAll win
  
  mainGUI
