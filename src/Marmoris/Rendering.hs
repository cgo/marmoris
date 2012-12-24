module Marmoris.Rendering where

import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.ForeignPtr (toForeignPtr)
import qualified Marmoris.Field as F
import Marmoris.Image
import Marmoris.Field (Field(..))
import Control.Monad (zipWithM, zipWithM_)
import Data.Array.MArray
import Data.Array
import Data.Word (Word8)
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.Core31
import Foreign.ForeignPtr.Safe

data SPosition = SPosition GLfloat GLfloat
data SSize     = SSize     GLfloat GLfloat


quad :: SPosition -> SSize -> [Vertex3 GLfloat]
quad (SPosition x y) (SSize w h) = 
  [ Vertex3 x       y       0
  , Vertex3 (x + w) y       0
  , Vertex3 (x + w) (y + h) 0
  , Vertex3 x       (y + h) 0 ]


stoneTextureCoords :: [TexCoord2 GLfloat]
stoneTextureCoords = [TexCoord2 0 0
                     ,TexCoord2 1 0
                     ,TexCoord2 1 1
                     ,TexCoord2 0 1]


type StoneTextures = [(F.StoneID, TextureObject)]

lookupStoneTexture :: StoneTextures -> F.StoneID -> Maybe TextureObject
lookupStoneTexture sts s = lookup s sts

defineStoneTextures :: Field -> IO StoneTextures
defineStoneTextures f = do
  let sd = stoneData f
      n  = length $ elems sd
  texNames <- genObjectNames n 
  zipWithM defTex (elems sd) texNames
  where defTex s@(F.Stone img sid) name = do
          rowAlignment Unpack $= 1
          textureBinding Texture2D $= Just name
          glGetError >>= \a -> print ("textureBinding: " ++ show a)
          print ("Binding image for stone " ++ show sid)
          bindImage img
          glGetError >>= \a -> print ("bindImage: " ++ show a)
          return (sid,name)

bindImage :: RGBAImg -> IO ()
bindImage img = do
  fp <- imgToPixelData img
  let (w,h) = imageSize img
  withForeignPtr fp $ \ptr -> do
    let pd = PixelData RGBA Byte ptr
    texImage2D Nothing NoProxy 0 RGBA8 (TextureSize2D (fromIntegral w) (fromIntegral h)) 0 pd


imgToPixelData :: RGBAImg -> IO (ForeignPtr Word8)
imgToPixelData img = do
  let (w,h) = imageSize img
      sz = w * h * 4
  --p <- mallocForeignPtrArray sz :: IO (ForeignPtr Word8)
  --let a = fromForeignPtr (Z :. h :. w :. 4) p
  toForeignPtr `fmap` R.copyP img
  
  -- return $ PixelData RGBA Byte ptr


renderStone :: TextureObject -> SPosition -> SSize -> IO ()
renderStone name p s = do
  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just name
  renderPrimitive Quads $ do
    color $ Color4 0 0 1 (1::GLfloat)
    zipWithM_ (\a b -> texCoord a >> vertex b) stoneTextureCoords (quad p s)
{-# INLINE renderStone #-}

-- Render all the stones to a canonical plane (which will be transformed by
-- the modelview matrix).
renderField :: StoneTextures -> Field -> IO ()
renderField stex (Field farray fstoneData fppos) = do
  texture Texture2D $= Enabled
  preservingMatrix $ do
    ((h0,w0),(h1,w1)) <- getBounds farray
    matrixMode $= Modelview 0
    scale
      (0.75 * 1 / fromIntegral (w1 - w0 + 1))
      (0.75 * 1 / fromIntegral (h1 - h0 + 1))
      (1 :: GLfloat)    
    as <- getAssocs farray
    mapM_ (\((x,y), b) -> let pos = SPosition (fromIntegral x) (fromIntegral y)
                          in renderStone (stl b) pos (SSize 1 1)) as
      where
        defaultTexture = TextureObject 0
        stl = maybe defaultTexture id . lookupStoneTexture stex
  