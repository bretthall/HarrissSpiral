{-# LANGUAGE  TemplateHaskell, Rank2Types#-}

module Main where

import Harriss
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events
import Data.IORef
import Control.Lens
import Debug.Trace (traceShow)

data GUIState = GUIState {_numGens::Int, _drawCurves::Bool, _drawSquares::Bool, _colorSquares::Bool, _drawSplitRects::Bool}
              deriving Show

$(makeLenses ''GUIState)

debugPrintM :: (Monad m, Show a) => a -> m ()
--debugPrintM a = traceShow a $ return ()
debugPrintM _ = return ()

makeCheckButton :: String -> Lens' GUIState Bool -> IORef GUIState -> VBox -> DrawingArea -> IO CheckButton
makeCheckButton text lens stateRef box plot = do
  button <- checkButtonNewWithMnemonic text
  boxPackStart box button PackNatural 0
  state <- readIORef stateRef
  toggleButtonSetActive button $ state^.lens
  on button buttonActivated (updateCheckedState button stateRef lens)
  return button
    where
      updateCheckedState :: CheckButton -> IORef GUIState -> Lens' GUIState Bool -> IO ()
      updateCheckedState button state lens = do
                                           checked <- toggleButtonGetActive button 
                                           modifyIORef state $ lens.~checked
                                           readIORef state >>= debugPrintM
                                           widgetQueueDraw plot
                                                                   
updateGeneration :: SpinButton -> IORef GUIState -> DrawingArea -> IO ()
updateGeneration sb state plot = do
  g  <- spinButtonGetValue sb
  modifyIORef state $ numGens.~(round g)
  readIORef state >>= debugPrintM
  widgetQueueDraw plot

plotScale :: Double
plotScale = 10000.0

data RectType = First | Second deriving (Show, Eq)
                      
radiusMult = 1.0

drawPlot :: DrawingArea -> IORef GUIState -> Event -> IO Bool
drawPlot plot guiState _ = do
  debugPrintM "****** drawing"
  win <- widgetGetDrawWindow plot
  (w, h) <- widgetGetSize plot
  state <- readIORef guiState
  renderWithDrawable win $ do
                     p0 <- deviceToUser 0.0 0.0 
                     debugPrintM $ "(0,0) = " ++ show p0
                     (uw, uh) <- deviceToUser (fromIntegral w) (fromIntegral h) 
                     debugPrintM $ "(w,h) = " ++ show (uw, uh)

                     translate 0.0 uh
                     scale (uw/plotScale)(-uw/plotScale)

                     ll <- deviceToUser 0.0 (fromIntegral h)
                     debugPrintM $ "lower left = " ++ show ll

                     ur <- deviceToUser (fromIntegral w) 0.0
                     debugPrintM $ "upper right = " ++ show ur
                     
                     let plotWidth = if snd ur < fst ur / p
                                     then snd ur * p
                                     else fst ur

                     setSourceRGB 1.0 1.0 1.0
                     paint

                     setLineWidth 10                     
                     setSourceRGB 0.0 0.0 0.0
                     drawGenerations $ take (state^.numGens) $ generations $ divisions plotWidth
  debugPrintM "****** done drawing"
  return True
      where
        drawGenerations (g:gs) = do 
                             drawGeneration g
                             if null gs
                                then mapM_ drawLastGeneration g
                                else drawGenerations gs
        drawGenerations [] = return ()
        drawGeneration g = do 
          debugPrintM "**** gen start" 
          mapM_ drawSquare g
          debugPrintM "**** gen end"
        drawLastGeneration (Div _ _ (r1, _) (r2, _)) = do
            debugPrintM "**** last gen start"
            drawRect First r1 
            drawRect Second r2
            debugPrintM "**** last gen end"
        drawSquare d@(Div e s _ _) = do
                             debugPrintM $ show e ++ show s 
                             strokeRect s
                             setSquareColor e
                             fillPreserve
                             setSourceRGB 0.0 0.0 0.0
                             stroke
                             let (a1, a2) = arcsForDivision radiusMult d
                             debugPrintM (a1, a2)
                             strokeArc a1
                             stroke
                             strokeArc a2
                             stroke
        -- probably would look better to use a bezier curve here, circular arcs have kinks at the corners of the squares
        strokeArc (Arc (P x y) r a1 a2) = arc x y r a1 a2
        drawRect t r = do
          debugPrintM $ show t ++ show r
          strokeRect r
          if t == First
             then setSourceRGB 0.0 0.0 1.0
             else setSourceRGB 0.0 1.0 0.0
          fillPreserve
          setSourceRGB 0.0 0.0 0.0
          stroke
        strokeRect (Rect (P x1 y1) (P x2 y2)) = Graphics.Rendering.Cairo.rectangle x1 y1 (x2 - x1) (y2 - y1)
        setSquareColor (C Lower Harriss.Left) = setSourceRGB 1.0 1.0 0.0
        setSquareColor (C Upper Harriss.Left) = setSourceRGB 1.0 0.0 1.0
        setSquareColor (C Upper Harriss.Right) = setSourceRGB 0.0 1.0 1.0
        setSquareColor (C Lower Harriss.Right) = setSourceRGB 1.0 0.0 0.0


main = do
  initGUI
  win <- windowNew
  onDestroy win mainQuit
  windowSetDefaultSize win 800 500

  vBox <- vBoxNew False 0
  containerAdd win vBox

  guiState <- newIORef $ GUIState 0 True False False False

  plot <- drawingAreaNew
  boxPackStart vBox plot PackGrow 0
  onExpose plot $ drawPlot plot guiState

  gen <- spinButtonNewWithRange 0.0 100000.0 1.0
  boxPackStart vBox gen PackNatural 0
  onValueSpinned gen (updateGeneration gen guiState plot)
  
  makeCheckButton "Draw _Curves" drawCurves guiState vBox plot
  makeCheckButton "Draw _Squares" drawSquares guiState vBox plot
  makeCheckButton "Color _Squares" colorSquares guiState vBox plot
  makeCheckButton "Draw _Split Rectangles" drawSplitRects guiState vBox plot

  widgetShowAll win
  widgetQueueDraw plot

  mainGUI
