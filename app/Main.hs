{-# LANGUAGE FlexibleContexts #-}

import Graphics.UI.Gtk hiding (fill, rectangle)
import Graphics.UI.Gtk.Gdk.EventM (EventM, EButton)
import Graphics.Rendering.Cairo
import Control.Monad (liftM, when, forM_)
import Data.IORef
import Data.Array.IO (newArray, readArray)
import System.Environment (getArgs)
import FlipIt (Board, size, flipPanel, reset, shuffle)

updateCanvas :: DrawingArea -> Board -> Board -> Int -> IORef Bool -> EventM any Bool
updateCanvas area board ans m cheat = do
  liftIO $ do win <- widgetGetDrawWindow area
              renderWithDrawable win $ drawBoard area board ans m cheat
  return True

updateBoard :: DrawingArea -> Board -> Board -> Int -> IORef Bool -> EventM EButton Bool
updateBoard area board ans m cheat = do
  liftIO $ do (x, y) <- widgetGetPointer area
              (sw, sh, pw, ph) <- boardGetLength area board
              flipPanel board ans m (div x (floor $ sw + pw), div y (floor $ sh + ph))
  updateCanvas area board ans m cheat

drawBoard :: DrawingArea -> Board -> Board -> Int -> IORef Bool -> Render ()
drawBoard area board ans m cheat = do
  setSourceRGB 0.5 0.5 0.5
  paint
  (sw, sh, pw, ph) <- liftIO $ boardGetLength area board
  n <- liftIO $ size board
  c <- liftIO.readIORef $ cheat
  forM_ (sequence $ replicate 2 [0..n-1]) $ \[i', j'] ->
        do let [i, j] = map fromIntegral [i', j']
           x <- liftIO.readRatio board $ (i', j')
           if x == 1
             then setSourceRGB 1 1 1
             else setSourceRGB (0.7 * x) (0.7 * x) x
           rectangle (sw * (i+1) + pw * i) (sh * (j+1) + ph * j) pw ph
           fill
           when c $ do
             y <- liftIO.readRatio ans $ (i', j')
             when (y < 1) $ do
               setSourceRGB (1 - y) 0.9 (0.6 - 0.6 * y)
               rectangle (sw * (i+6) + pw * i) (sh * (j+6) + ph * j) (pw - 10*sw) (ph - 10* sh)
               fill
    where ratio x = fromIntegral (m - 1 - x) / fromIntegral (m - 1)
          readRatio array = liftM ratio.readArray array


boardGetLength :: DrawingArea -> Board -> IO (Double, Double, Double, Double)
boardGetLength area board = do
  (_w, _h) <- widgetGetSize area
  n <- liftM fromIntegral $ size board
  let w = fromIntegral _w
      h = fromIntegral _h
      space = 0.05 / (n + 1)
      panel = 0.95 / n
  return (space * w, space * h, panel * w, panel * h)


run :: Int -> Int -> IO ()
run _m n = do
  let m | notElem _m [2,3,5,7] = 2
        | otherwise            = _m
  initGUI
  -- win (Window)
  -- +--- vbox (VBox)
  --      +--- can (DrawingArea)
  --      +--- hbox (HBox)
  --           +-- cls (Button)
  --           +-- rst (Button)
  --           +-- shf (Button)
  --           +-- cht (Button)
  win <- windowNew
  win `set` [windowTitle := "FlipIt!",
             windowDefaultWidth := n * 50,
             windowDefaultHeight := n * 50 + 40,
             containerBorderWidth := 0]
  win `onDestroy` mainQuit
  vbox <- vBoxNew False 0
  can <- drawingAreaNew
  hbox <- hBoxNew False 0
  cls <- buttonNewWithLabel "Close"
  rst <- buttonNewWithLabel "Reset"
  shf <- buttonNewWithLabel "Shuffle"
  cht <- buttonNewWithLabel "Cheat"

  containerAdd win vbox
  boxPackStart vbox can PackGrow 0
  boxPackStart vbox hbox PackNatural 5
  boxPackStart hbox cls PackGrow 0
  boxPackStart hbox rst PackGrow 0
  boxPackStart hbox shf PackGrow 0
  boxPackStart hbox cht PackGrow 0
  widgetShowAll win

  board <- newArray ((0, 0), (n-1, n-1)) 0
  ans <- newArray ((0, 0), (n-1, n-1)) 0
  cheat <- newIORef False
  on can exposeEvent $ updateCanvas can board ans m cheat
  on can buttonPressEvent $
     updateBoard can board ans m cheat
  on cls buttonPressEvent $
     liftIO mainQuit >> return True
  on rst buttonPressEvent $
     liftIO (reset board ans) >> updateBoard can board ans m cheat
  on shf buttonPressEvent $
     liftIO (shuffle board ans m) >> updateBoard can board ans m cheat
  on cht buttonPressEvent $
     liftIO (modifyIORef cheat not) >> updateBoard can board ans m cheat
  mainGUI

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then run 2 5
    else run (read $ args!!0) (read $ args!!1)
