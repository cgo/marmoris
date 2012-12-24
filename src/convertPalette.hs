module Main where

import Char
import Control.Monad (liftM)
import Data.Colour.SRGB
import Data.Word

digitsToInt :: [Char] -> Int
digitsToInt as = snd $ foldl f (1, 0) $ reverse as
  where
    f (b, c) a = (b * 16, c + b * (digitToInt a))

digitsToRGB :: [Char] -> RGB Word8
digitsToRGB as = RGB r g b
  where
    r : g : b : [] = map fromIntegral $ f as
    f [] = []
    f as = digitsToInt as1 : f as2
      where (as1, as2) = ((take 2 as), (drop 2 as))

main = do
  p <- liftM words $ readFile "vgapalette.txt"
  let rgbs = map digitsToRGB p
  writeFile "vgapalette.hs" $ show rgbs  
  --putStrLn $ show rgbs
  --putStrLn $ "Length: " ++ show (length rgbs)