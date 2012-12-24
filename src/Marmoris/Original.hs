module Marmoris.Original where

import Marmoris.Field
import Marmoris.Image
import Marmoris.VGAPalette
import qualified Data.ByteString as B
import Data.Colour.SRGB

import System.Environment
import qualified Data.Array as A
import qualified Data.Array.Repa as R
import Data.Array.Repa
import Data.Array.Repa.IO.DevIL
import Control.Monad (liftM)
import Debug.Trace


loadPalette :: FilePath -> IO Palette
loadPalette fname = do
  p <- B.readFile fname
  let l = reverse $ createPalette (B.unpack p) []
  return $ A.listArray (0, length l - 1) l
    
createPalette [] p = p
createPalette (r:g:b:rest) p = createPalette rest (RGB r g b : p)

{-| Scale palette to (0,255). -}
scalePalette :: Palette -> Palette
scalePalette p = A.listArray (0, length pl - 1) pl
  where pl = reverse $ createPalette l' []
        l = Prelude.map realToFrac $ paletteToList p
        min = minimum l
        max = maximum l
        l' = Prelude.map f l
        f x = floor $ (x - min) / (max - min) * 255



imgsFromStones :: Palette -> B.ByteString -> [RGBAImg]
imgsFromStones pal s = reverse $ f s []
  where
    sz = R.Z R.:. 19 R.:. 25
    f s imgs | B.null s = imgs
             | otherwise = f s'' (i : imgs)
      where (s',s'') = B.splitAt 475 s
            i = scalarToRGBAImg pal (B.unpack s') 25 19


convertOldStones :: Palette -> FilePath -> IO [Stone]
convertOldStones pal fname = do
  a <- B.readFile fname
  let imgs = imgsFromStones pal a
  putStrLn $ "Number of stones read: " Prelude.++ show (length imgs)
  mapM_ (\(n, i) -> runIL $ writeImage (show n Prelude.++ ".bmp") i) $ zip [1,2..] imgs
  return []
  
