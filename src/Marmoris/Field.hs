{-# LANGUAGE BangPatterns #-}

module Marmoris.Field 
       ( StoneID,
         toStoneID,
         fromStoneID,
         StoneType(..),
         Position,
         FieldArray,
         Field(..),
         Stone(..),
         loadStones,
         loadField,
         makeEmptyField,
         module Data.Word, 
         module Data.Array.IO )  
       where

import Data.Array.IO
import Data.Array
import Data.Word
import qualified Data.ByteString as B
import Marmoris.Image
import Data.List (sortBy)

type Position = (Int,Int)

type FieldArray = IOUArray Position StoneID

data Field = Field { fieldData :: FieldArray,
                     stoneData :: Array StoneID Stone,
                     playerPositions :: IOArray Int Position }
             
type StoneID = Int
             
data Stone = Stone { stoneImg :: RGBAImg, 
                     stoneID :: !StoneID } deriving (Show, Eq)

makeEmptyField :: IO Field
makeEmptyField = do
  fd <- newListArray ((0,0),(4,4)) $ repeat 1
  stones <- loadStones
  pp <- newListArray (0,1) [(0,0),(0,1)]
  return $ Field { fieldData = fd
                 , stoneData = listArray (1,length stones) stones
                 , playerPositions = pp }

data StoneType = Regular
               | Acid
               | RiverLR
               | RiverRL
               | Wall
               | ReplacementTile
               | Empty
               | Loose
               | Gold
               | Island
               | Suitcase
               | Indestructible
               | Runner
               | Player1
               | Player2
                 deriving (Eq, Show, Enum, Ord)

toStoneID :: StoneType -> StoneID
toStoneID = (+1) . fromEnum

fromStoneID :: StoneID -> StoneType
fromStoneID = toEnum . subtract 1

loadStones :: IO [Stone]
loadStones = sortBy (\(Stone _ n1) (Stone _ n2) -> compare n1 n2) `fmap`
             mapM f [1,2..42]
  where
    f n = loadRGBAImg (show n ++ ".bmp") >>= \s -> return (Stone s n)
    

{-| For now, simply reads the old level files. -}
loadField :: FilePath -> IO Field
loadField levelName = do
  stones <- loadStones  
  fieldArray <- loadOldLevel levelName
  pos <- loadOldPlayerPositions levelName >>= newListArray (0,1)
  return $ Field { fieldData = fieldArray,
                   stoneData = listArray (1, length stones) stones,
                   playerPositions = pos }
                             
                             
{-| Load a level from the original X86 ASM game. -}
loadOldLevel :: FilePath -> IO FieldArray
loadOldLevel fname = do
  l <- B.readFile fname
  a <- newListArray ((0, 0),(89,89)) $ map fromIntegral $ take (90 * 90) $ B.unpack l
  return a
  

loadOldPlayerPositions :: FilePath -> IO [Position]
loadOldPlayerPositions fname = do
  l <- B.readFile fname
  let 
      toInt = fromIntegral . fromEnum
      p1 = (toInt $ l `B.index` 8100, toInt $ l `B.index` 8101)
      p2 = (toInt $ l `B.index` 8102, toInt $ l `B.index` 8103)
  return [p1,p2]

  
