{-# LANGUAGE ForeignFunctionInterface #-}

module FlipIt (Board, size, inBoard, flipPanel, reset, shuffle) where
import Control.Monad (liftM, when, forM_, replicateM, zipWithM_)
import Data.Array.IO (IOUArray, readArray, writeArray, getBounds, range, getElems)
import System.Random (randomRIO)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (newArray, mallocArray, peekArray)

-- Monadic when and if
whenM :: Monad m => m Bool -> m () -> m ()
whenM b f = b >>= flip when f

type Board = IOUArray (Int, Int) Int

size :: Board -> IO Int
size = liftM (succ.fst.snd).getBounds

inBoard :: Board -> (Int, Int) -> IO Bool
inBoard board (x, y) = do
  n <- size board
  return $ 0 <= x && x < n && 0 <= y && y < n


flipPanel :: Board -> Board -> Int -> (Int, Int) -> IO ()
flipPanel board ans m (x, y) = flipPanel_ board m (x, y) >> updateAns board ans m

flipPanel_ :: Board -> Int -> (Int, Int) -> IO ()
flipPanel_ board m (x, y) = do
  whenM (inBoard board (x, y)) $
        forM_ [(x, y), (x+1, y), (x, y+1), (x-1, y), (x, y-1)] $ \ix ->
            whenM (inBoard board ix) $
                  writeArray board ix.(`mod` m).succ =<< readArray board ix

reset :: Board -> Board -> IO ()
reset board ans = do
  bounds <- getBounds board
  forM_ (range bounds) $ \ix -> writeArray board ix 0 >> writeArray ans ix 0

shuffle :: Board -> Board -> Int -> IO ()
shuffle board ans m = do
  n <- size board
  r <- randomRIO (n, n*n)
  [xs, ys] <- replicateM 2.replicateM r $ randomRIO (0, n-1)
  mapM_ (flipPanel_ board m) $ zip xs ys
  updateAns board ans m

foreign import ccall "flipItSolver.h solve" c_solve ::
    Ptr CInt -> Ptr CInt -> CInt -> CInt -> IO CInt

updateAns :: Board -> Board -> Int -> IO ()
updateAns board ans m = do
  n <- size board
  b <- newArray.map fromIntegral =<< getElems board
  x <- mallocArray $ n * n
  c_solve x b (fromIntegral n) (fromIntegral m)
  xs <- peekArray (n * n) x
  bounds <- getBounds ans
  zipWithM_ (writeArray ans) (range bounds) (map fromIntegral xs)
  free b
  free x
