module NA.BMP
    ( readBMP
    , writeBMP
    , Image (..)
    , channelToDouble
    , channelToWord8
    )
where

import Data.Array.ST
import Data.Array.Unboxed
import qualified Codec.BMP as C
import Data.ByteString as BS
import Data.Word

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
  let ((w0, h0), (w1, h1)) = bounds r
  let bmp = C.packRGBA32ToBMP (w1-w0+1) (h1-h0+1) rgba
  C.writeBMP fileName bmp

channelToDouble :: Channel Word8 -> Channel Double
channelToDouble = amap fromIntegral

channelToWord8 :: Channel Double -> Channel Word8
channelToWord8 = amap truncate

toImage :: ByteString -> Int -> Int -> (Image Word8, Int, Int)
toImage rgba width height = (Image reds greens blues, width, height)
    where (reds, greens, blues) =  toChannels rgba
          getRGB (r:g:b:_:rest) = (r, g, b) : getRGB rest
          getRGB _              = []
          ranges = ((1,1), (width, height))
          getArray r = array r . Prelude.zip (range r)
          toChannels = apply3 (getArray ranges) . unzip3 . getRGB . BS.unpack

toList :: Image Word8 -> [Word8]
toList (Image red green blue) = toListAux (elems red) (elems green) (elems blue)
    where toListAux (r:rs) (g:gs) (b:bs) = r:g:b:0:toListAux rs gs bs
          toListAux _      _      _      = []

apply3 :: (t -> t1) -> (t, t, t) -> (t1, t1, t1)
apply3 f (a, b, c) = (f a, f b, f c)
