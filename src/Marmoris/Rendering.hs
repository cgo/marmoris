module Marmoris.Rendering where

import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.ForeignPtr (toForeignPtr)
import qualified Marmoris.Field as F
import Marmoris.Image
import Marmoris.Field (Field(..))
import Control.Concurrent (threadDelay)
import Control.Monad (zipWithM, zipWithM_,when)
import Data.Array.MArray
import Data.Array
import Data.Maybe (catMaybes)
import Data.Word (Word8)
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.Core31
import Foreign.ForeignPtr.Safe
import System.Posix.Unistd

data SPosition = SPosition GLfloat GLfloat
data SSize     = SSize     GLfloat GLfloat


quad :: SPosition -> SSize -> [Vertex3 GLfloat]
quad (SPosition x y) (SSize w h) = 
  [ Vertex3 x       y       0
  , Vertex3 (x + w) y       0
  , Vertex3 (x + w) (y + h) 0
  , Vertex3 x       (y + h) 0 ]


stoneTextureCoords :: [TexCoord2 GLfloat]
stoneTextureCoords = [TexCoord2 0 1
                     ,TexCoord2 1 1
                     ,TexCoord2 1 0
                     ,TexCoord2 0 0]


type StoneTextures = [(F.StoneID, TextureObject)]

lookupStoneTexture :: StoneTextures -> F.StoneID -> Maybe TextureObject
lookupStoneTexture sts s = lookup s sts

defineStoneTextures :: Field -> IO StoneTextures
defineStoneTextures f = do
  let sd = stoneData f
      n  = length $ elems sd
      
  texture Texture2D $= Enabled
  activeTexture $= TextureUnit 0
  textureWrapMode Texture2D S $= (Repeated, Clamp)
  textureWrapMode Texture2D T $= (Repeated, Clamp)
  textureWrapMode Texture2D R $= (Repeated, Clamp)
  textureFilter Texture2D $= ((Linear', Nothing), Linear') -- This is essential!
  generateMipmap Texture2D $= Disabled

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
    let pd = PixelData RGBA UnsignedByte ptr
    texImage2D Nothing NoProxy 0 RGBA8 (TextureSize2D (fromIntegral w) (fromIntegral h)) 0 pd


imgToPixelData :: RGBAImg -> IO (ForeignPtr Word8)
imgToPixelData img = do
  let (w,h) = imageSize img
      sz = w * h * 4
  toForeignPtr `fmap` R.copyP img
  

initRendering :: IO ()
initRendering = do
  clearColor $= Color4 0 0 0 1
  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 1 0 1  
  matrixMode $= Modelview 0
  loadIdentity


setTexturing :: IO ()
setTexturing = do
  texture Texture2D $= Enabled
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  textureFunction $= Decal


renderStone :: TextureObject -> SPosition -> SSize -> IO ()
renderStone name p s = do
  setTexturing
  textureBinding Texture2D $= Just name
  glGetError >>= \e -> when (e /= 0) $ print ("renderStone: " ++ show e)
  renderPrimitive Quads $ do
    -- color $ Color4 0 0 1 (1::GLfloat)
    -- normal $ Normal3 0 0 (1::GLfloat)
    zipWithM_ (\a b -> texCoord a >> vertex b) stoneTextureCoords (quad p s)
  texture Texture2D $= Disabled
{-# INLINE renderStone #-}


animateSequence :: StoneTextures -> Field -> SPosition -> SSize -> [F.StoneID] -> IO () -> IO ()
animateSequence stex field p@(SPosition x y) s@(SSize w h) ids showAction = do
  initRendering
  preservingMatrix $ do
    let f i = do
          writeArray (fieldData field) (round x,round y) i
          preservingMatrix $ initRendering >> renderField stex field
          --setTexturing
          --textureBinding Texture2D $= Just name
          --clear [ColorBuffer, DepthBuffer]
          --renderPrimitive Quads $
          --  zipWithM_ (\a b -> texCoord a >> vertex b) stoneTextureCoords (quad p s)
          --texture Texture2D $= Disabled
          --flush
          showAction
          threadDelay . fromIntegral $ (4000 `div` (length ids))
          -- nanosleep . fromIntegral $ (2000 `div` (length ids)) * 10^6
    let names = catMaybes $ map (flip lookup stex) ids
    print $ "Names: " ++ show names
    fieldTransformation field
    a <- readArray (fieldData field) (round x,round y)
    mapM_ f ids
    writeArray (fieldData field) (round x,round y) a
    


-- | Apply the transformation so that each stone goes to its position
--   and has size (1,1).
fieldTransformation :: Field -> IO ()
fieldTransformation (Field farray _ _)= do
  ((h0,w0),(h1,w1)) <- getBounds farray
  matrixMode $= Modelview 0
  let tx = fromIntegral (w1 - w0 + 1) / 2
      ty = fromIntegral (h1 - h0 + 1) / 2
  translate $ Vector3 0.5 0.5 (0 :: GLfloat)
  scale
    (0.75 * 1 / fromIntegral (w1 - w0 + 1))
    (0.75 * 1 / fromIntegral (h1 - h0 + 1))
    (1 :: GLfloat)    
  translate $ Vector3 (-tx) (-ty) (0 :: GLfloat)


-- Render all the stones to a canonical plane (which will be transformed by
-- the modelview matrix).
renderField :: StoneTextures -> Field -> IO ()
renderField stex field@(Field farray fstoneData fppos) = do
  preservingMatrix $ do
    fieldTransformation field
    as <- getAssocs farray
    mapM_ (\((x,y), b) -> let pos = SPosition (fromIntegral x) (fromIntegral y)
                          in renderStone (stl b) pos (SSize 1 1)) as
    pp <- getElems fppos >>= return . map (\(x,y) -> SPosition (fromIntegral x) (fromIntegral y))
    zipWithM_ (\a p -> renderStone (stl (F.toStoneID a)) p (SSize 1 1)) [F.Player1,F.Player2] pp
    flush
      where
        defaultTexture = TextureObject 0
        stl = maybe defaultTexture id . lookupStoneTexture stex
  