module NA.Image
    ( Coordinate
    , Channel
    , Image (..)
    , readBMP
    , writeBMP
    , imap
    , cfold
    , cfold1
    )
where

import Data.Word
import qualified Data.List as L
import Data.Array.Unboxed
import Data.ByteString as BS
import qualified Codec.BMP as C

type Coordinate = (Int, Int)

type Channel a = UArray Coordinate a

data Image a = Image { redChannel, greenChannel, blueChannel :: Channel a}

readBMP :: FilePath -> IO (Image Word8, Int, Int)
readBMP fileName = do
  Right bmp  <- C.readBMP fileName
  let rgba = C.unpackBMPToRGBA32 bmp
  let (width, height) = C.bmpDimensions bmp
  return $ toImage rgba width height

writeBMP :: FilePath -> Image Word8 -> IO ()
writeBMP fileName img@(Image r _ _) = do
  let rgba = BS.pack . toList $ img
  let ((h0, w0), (h1, w1)) = bounds r
  let bmp = C.packRGBA32ToBMP (w1-w0+1) (h1-h0+1) rgba
  C.writeBMP fileName bmp

imap :: (Channel a -> Channel b) -> Image a -> Image b
imap f (Image r g b) = Image (f r) (f g) (f b)

cfold :: (Ix i, IArray a b) => (c -> b -> c) -> c -> a i b -> c
cfold f ini = L.foldl' f ini . elems

cfold1 :: (Ix i, IArray a c) => (c -> c -> c) -> a i c -> c
cfold1 f = L.foldl1' f . elems

toImage :: ByteString -> Int -> Int -> (Image Word8, Int, Int)
toImage rgba width height = (Image reds greens blues, width, height)
    where (reds, greens, blues) =  toChannels rgba
          getRGB (r:g:b:_:rest) = (r, g, b) : getRGB rest
          getRGB _              = []
          ranges = ((1,1), (height, width))
          getArray r = array r . Prelude.zip (range r)
          toChannels = apply3 (getArray ranges) . unzip3 . getRGB . BS.unpack

toList :: Image Word8 -> [Word8]
toList (Image red green blue) = toListAux (elems red) (elems green) (elems blue)
    where toListAux (r:rs) (g:gs) (b:bs) = r:g:b:0:toListAux rs gs bs
          toListAux _      _      _      = []

apply3 :: (t -> t1) -> (t, t, t) -> (t1, t1, t1)
apply3 f (a, b, c) = (f a, f b, f c)
