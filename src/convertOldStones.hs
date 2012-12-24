module Main where

import Marmoris.Original
import Marmoris.Field
import Marmoris.VGAPalette
import Marmoris.Image
import System.Environment
import qualified Data.Array.Repa as R
import Data.Array.Repa.IO.DevIL
import Control.Monad (liftM)


main :: IO ()
main = do
  pal <- scalePalette `fmap` loadPalette "../original/palette"
  convertOldStones pal "../original/steine.gme"
  return ()