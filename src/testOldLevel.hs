module Main where

import Marmoris.Original
import Marmoris.Field
import Marmoris.VGAPalette
import Marmoris.Image
import Data.Array
import System.Environment
import qualified Data.Array.Repa as R
import Data.Array.Repa.IO.DevIL
import Control.Monad (liftM)


main :: IO ()
main = do
  [fn] <- getArgs
  Field f _ pos <- loadField fn
  putStrLn $ "Player positions: " ++ show (pos)
  ps <- liftM (words) $ readFile "vgapalette.txt"
  putStrLn (show ps)
  bds@(_,(wm1,hm1)) <- getBounds f
  putStrLn $ "Extent: " ++ show bds
  
  f' <- freeze f :: IO (Array Position StoneID)
  els <- map fromIntegral `fmap` getElems f
  let rgb = scalarToRGBAImg vgaPaletteArr els (wm1+1) (hm1+1)
  putStrLn $ "Extent of the RGB image: " ++ show (R.arrayExtent rgb)
  runIL $ writeImage "level.bmp" rgb
  
  return ()