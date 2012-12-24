module Marmoris.Image
       ( Img, 
         RGBAImg,
         imageSize,
         scalarToRGB, 
         scalarToRGBAImg, 
         loadRGBAImg ) where

import Data.Array.Repa.Eval (fromList)
import qualified Data.Array.Repa as R
import Data.Array.Repa.Index
import Marmoris.VGAPalette
import qualified Data.ByteString as B
import Data.Colour.SRGB
import Data.Word
import Data.Array.Unboxed
import Codec.Image.DevIL

type Img = R.Array R.U R.DIM2 Word8
type RGBAImg = R.Array R.U R.DIM3 Word8

imageSize :: RGBAImg -> (Int, Int)
imageSize img = (w,h)
  where Z :. h :. w :. chan = R.extent img

scalarToRGB :: Palette -> Integral a => a -> RGB Word8
scalarToRGB pal a = pal ! (fromIntegral a)

{-| Take a scalar Img and using Marmoris.VGAPalette, create an RGBAImg. -}
scalarToRGBAImg :: Palette -> [Word8] -> Int -> Int -> RGBAImg
scalarToRGBAImg pal img w h = fromList s' $ concat [[x,y,z,255] | (x,y,z) <- zip3 r g b]
  where a = map (scalarToRGB pal) img 
        s = R.Z R.:. h R.:. w
        s' = R.shapeOfList $ 4 : R.listOfShape s
        r = map channelRed a
        g = map channelGreen a
        b = map channelBlue a

loadRGBAImg :: FilePath -> IO RGBAImg
loadRGBAImg fname = ilInit >>
                    readImage fname >>= \i ->
                    let (_,(h,w,c)) = bounds i
                        s = R.Z R.:. (h+1) R.:. (w+1) R.:. (c+1)
                    in return $ fromList s $ elems i