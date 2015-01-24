{-# LANGUAGE  TemplateHaskell, Rank2Types#-}

module Main where

import Harriss
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events
import Data.IORef
import Control.Lens
import Debug.Trace (traceShow)

data GUIState = GUIState {_numGens::Int, 
                          _drawCurves::Bool, 
                          _drawSquares::Bool, 
                          _colorSquares::Bool, 
                          _drawSplitRects::Bool,
                          _lineThickness::Double,
                          _entryAngleDegrees::Angle,
                          _magMult::MagnitudeMult}
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

parseDouble :: String -> Maybe Double
parseDouble s = parse $ reader s
    where
      reader = (readsPrec 0)::(ReadS Double)
      parse [(n, leftover)] | null leftover = Just n
                            | otherwise = Nothing
      parse _ = if (head s) == '.'
                then parse $ reader $ '0':s
                else Nothing
               
makeNumEntry :: String -> Lens' GUIState Double -> IORef GUIState -> VBox -> DrawingArea -> IO Entry
makeNumEntry text lens stateRef vBox plot = do  
  hBox <- hBoxNew False 1
  boxPackStart vBox hBox PackNatural 0
  label <- labelNew $ Just text
  boxPackStart hBox label PackNatural 0
  entry <- entryNew
  boxPackStart hBox entry PackNatural 0
  state <- readIORef stateRef 
  entrySetText entry $ show $ state^.lens
  needRedrawRef <- newIORef False
  idInsertRef <- newIORef undefined
  id <- entry `on` insertText $ \str pos -> do
          debugPrintM "Adding text"
          curText <- entryGetText entry
          debugPrintM $ "old string = " ++ curText
          let newText = (take pos curText) ++ str ++ (drop pos curText)
          debugPrintM $ "new string = " ++ newText
          case parseDouble newText of
            Just n -> do
              debugPrintM $ "new value = " ++ show n
              modifyIORef stateRef $ lens.~n
              modifyIORef needRedrawRef $ const True
              id <- readIORef idInsertRef
              signalBlock id
              pos' <- editableInsertText entry str pos
              signalUnblock id
              stopInsertText id
              return pos'
            Nothing -> do
              debugPrintM "bad value"
              id <- readIORef idInsertRef
              signalBlock id
              pos' <- editableInsertText entry "" pos
              signalUnblock id
              stopInsertText id
              return pos'
  writeIORef idInsertRef id
  idDeleteRef <- newIORef undefined
  dd <- entry `on` deleteText $ \start end -> do
          debugPrintM "Deleting text"
          curText <- entryGetText entry
          debugPrintM $ "old string = " ++ curText
          let newText = (take start curText) ++ (drop end curText)
          debugPrintM $ "new string = " ++ newText
          case parseDouble newText of
            Just n -> do
              debugPrintM $ "new value = " ++ show n
              modifyIORef stateRef $ lens.~n
              modifyIORef needRedrawRef $ const True
              id <- readIORef idDeleteRef
              signalBlock id
              pos' <- editableDeleteText entry start end
              signalUnblock id
              stopDeleteText id
            Nothing -> do
              debugPrintM "bad value"
              id <- readIORef idDeleteRef
              signalBlock id
              pos' <- editableDeleteText entry start start
              signalUnblock id
              stopDeleteText id
  writeIORef idDeleteRef dd
  entry `on` focusOutEvent $ do
    needRedraw <- liftIO $ readIORef needRedrawRef
    if needRedraw
       then (liftIO $ widgetQueueDraw plot) >> (liftIO $ modifyIORef needRedrawRef (const False))
       else return ()
    return False
  entry `on` entryActivate $ do
    debugPrintM "activate" 
    needRedraw <- readIORef needRedrawRef
    if needRedraw
       then widgetQueueDraw plot >> modifyIORef needRedrawRef (const False)
       else return ()
  return entry

plotScale :: Double
plotScale = 10000.0

data RectType = First | Second deriving (Show, Eq)

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

                     setLineWidth $ state^.lineThickness
                                  
                     let getCurves = curvesForDivision ((state^.entryAngleDegrees)*pi/180) (state^.magMult)

                     setSourceRGB 0.0 0.0 0.0
                     if state^.numGens == 0
                        then if state^.drawSplitRects then drawGen0Rect plotWidth else return ()
                        else drawGenerations state getCurves $ take (state^.numGens) $ generations $ divisions plotWidth
  debugPrintM "****** done drawing"
  return True
      where
        drawGen0Rect w = drawRect First (Rect (P 0 0) (P w (w/p)))
        drawGenerations state getCurves (g:gs) = do 
                             drawGeneration state getCurves g
                             if null gs
                                then mapM_ (drawLastGeneration state) g
                                else drawGenerations state getCurves gs
        drawGenerations _ _ [] = return ()
        drawGeneration state getCurves g = do 
          debugPrintM "**** gen start" 
          mapM_ (drawSquare state getCurves) g
          debugPrintM "**** gen end"
        drawLastGeneration state (Div _ _ (r1, _) (r2, _)) = do
            debugPrintM "**** last gen start"
            if state^.drawSplitRects 
            then do
              drawRect First r1 
              drawRect Second r2
            else
                return ()
            debugPrintM "**** last gen end"
        drawSquare state getCurves d@(Div e s _ _) = do
                             debugPrintM $ show e ++ show s 
                             if state^.drawSquares
                             then do
                               strokeRect s
                               if state^.colorSquares
                               then do
                                 setSquareColor e
                                 fillPreserve
                               else
                                   return ()
                               setSourceRGB 0.0 0.0 0.0
                               stroke
                             else
                                 return ()
                             let (c1, c2) = getCurves d
                             debugPrintM (c1, c2)                             
                             if state^.drawCurves
                             then do
                               bCurve c1
                               bCurve c2
                               stroke
                             else
                                 return ()
        bCurve (BCurve (P x0 y0) (P x1 y1) (P x2 y2) (P x3 y3)) = do
                             moveTo x0 y0
                             curveTo x1 y1 x2 y2 x3 y3
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

  hBox <- hBoxNew False 0
  containerAdd win hBox

  guiState <- newIORef $ GUIState 0 True False False False 25.0 45.0 0.41

  plot <- drawingAreaNew
  boxPackStart hBox plot PackGrow 0
  onExpose plot $ drawPlot plot guiState

  vBox <- vBoxNew False 0
  boxPackStart hBox vBox PackNatural 3

  genBox <- hBoxNew False 0
  boxPackStart vBox genBox PackNatural 0  
  genLabel <- labelNew $ Just "Generation"
  boxPackStart genBox genLabel PackNatural 0
  gen <- spinButtonNewWithRange 0.0 100000.0 1.0
  boxPackStart genBox gen PackNatural 0
  onValueSpinned gen (updateGeneration gen guiState plot)
  
  makeCheckButton "Draw _Curves" drawCurves guiState vBox plot
  makeCheckButton "Draw _Squares" drawSquares guiState vBox plot
  makeCheckButton "Color _Squares" colorSquares guiState vBox plot
  makeCheckButton "Draw _Split Rectangles" drawSplitRects guiState vBox plot

  makeNumEntry "Line Width" lineThickness guiState vBox plot
  makeNumEntry "Entry Angle (degrees)" entryAngleDegrees guiState vBox plot
  makeNumEntry "Mag Multiplier" magMult guiState vBox plot

  widgetShowAll win
  widgetQueueDraw plot

  mainGUI
