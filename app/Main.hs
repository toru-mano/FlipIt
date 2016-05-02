import Graphics.UI.Gtk hiding (fill, rectangle)
import Graphics.UI.Gtk.Gdk.EventM (EventM, EButton)
import Graphics.Rendering.Cairo
import Control.Monad (liftM, when, forM_, replicateM)
import Data.Array.IO (IOUArray, newArray, readArray, writeArray, getBounds, range)
import System.Environment (getArgs)
import System.Random (randomRIO)

type Board = IOUArray (Int, Int) Bool

size :: Board -> IO Int
size = liftM (succ.fst.snd).getBounds

inBoard :: Board -> (Int, Int) -> IO Bool
inBoard board (x, y) = do
  n <- size board
  return $ 0 <= x && x < n && 0 <= y && y < n

whenM :: Monad m => m Bool -> m () -> m ()
whenM b f = b >>= flip when f

ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM b t f = do _b <- b
               if _b then t else f

flipPanel :: Board -> (Int, Int) -> IO ()
flipPanel board (x, y) =
    whenM (inBoard board (x, y)) $
    forM_ [(x, y), (x+1, y), (x, y+1), (x-1, y), (x, y-1)] $ \ix ->
        whenM (inBoard board ix) $
        writeArray board ix.not =<< readArray board ix

reset :: Board -> IO ()
reset board = do
  bounds <- getBounds board
  forM_ (range bounds) $ \ix -> writeArray board ix False

shuffle :: Board -> IO ()
shuffle board = do
  n <- size board
  r <- randomRIO (n, n*n)
  [xs, ys] <- replicateM 2.replicateM r $ randomRIO (0, n-1)
  mapM_ (flipPanel board) $ zip xs ys

updateCanvas :: DrawingArea -> Board -> EventM any Bool
updateCanvas area board = do
  liftIO $ do win <- widgetGetDrawWindow area
              renderWithDrawable win $ drawBoard area board
  return True

updateBoard :: DrawingArea -> Board -> EventM EButton Bool
updateBoard area board = do
  liftIO $ do (x, y) <- widgetGetPointer area
              (sw, sh, pw, ph) <- boardGetLength area board
              flipPanel board (div x (floor $ sw + pw), div y (floor $ sh + ph))
  updateCanvas area board

drawBoard :: DrawingArea -> Board -> Render ()
drawBoard area board = do
  setSourceRGB 0.5 0.5 0.5
  paint
  (sw, sh, pw, ph) <- liftIO $ boardGetLength area board
  n <- liftIO $ size board
  forM_ (sequence $ replicate 2 [0..n-1]) $ \[i', j'] ->
        do let [i, j] = map fromIntegral [i', j']
           ifM (liftIO.readArray board $ (i', j')) (setSourceRGB 0 0 0) (setSourceRGB 1 1 1)
           rectangle (sw * (i+1) + pw * i) (sh * (j+1) + ph * j) pw ph
           fill

boardGetLength :: DrawingArea -> Board -> IO (Double, Double, Double, Double)
boardGetLength area board = do
  (_w, _h) <- widgetGetSize area
  n <- liftM fromIntegral $ size board
  let w = fromIntegral _w
      h = fromIntegral _h
      panel = (1 - (n + 1) * space) / n
  return (space * w, space * h, panel * w, panel * h)
      where space = 0.01

run n = do
  initGUI
  -- win (Window)
  -- +--- vbox (VBox)
  --      +--- can (DrawingArea)
  --      +--- hbox (HBox)
  --           +-- cls (Button)
  --           +-- rst (Button)
  --           +-- shf (Button)
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

  containerAdd win vbox
  boxPackStart vbox can PackGrow 0
  boxPackStart vbox hbox PackNatural 5
  boxPackStart hbox cls PackGrow 0
  boxPackStart hbox rst PackGrow 0
  boxPackStart hbox shf PackGrow 0
  widgetShowAll win

  board <- newArray ((0, 0), (n-1, n-1)) False
  on can exposeEvent $ updateCanvas can board
  on can buttonPressEvent $ updateBoard can board
  on cls buttonPressEvent $ liftIO mainQuit >> return True
  on rst buttonPressEvent $ liftIO (reset board) >> updateBoard can board
  on shf buttonPressEvent $ liftIO (shuffle board) >> updateBoard can board
  mainGUI

main = do
  args <- getArgs
  if length args > 0 then run.read.head $ args else run 5
