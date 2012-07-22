module NA.Image.Utils
    ( channelToDouble
    , channelToWord8
    , duplicate
    )
where

import NA.Image
import Control.Monad
import Data.Word
import Data.Array.ST
import Data.Array.Unboxed

channelToDouble :: Channel Word8 -> Channel Double
channelToDouble = amap fromIntegral

channelToWord8 :: Channel Double -> Channel Word8
channelToWord8 = amap truncate

duplicate :: Image Word8 -> Image Word8 -> Image Word8
duplicate (Image r1 g1 b1) (Image r2 g2 b2) = Image r g b
    where r = duplicateChannel r1 r2
          g = duplicateChannel g1 g2
          b = duplicateChannel b1 b2

duplicateChannel :: Channel Word8 -> Channel Word8 -> Channel Word8
duplicateChannel c1 c2 = runSTUArray $ do
                           dup <- newArray ((h0, w0), (h1, width)) 0
                           forM_ (Prelude.reverse $ range ((h0,w0), (h1,w1))) $ \(y,x) -> do
                                         let x' = x + (w1-w0+1) + 4 + w0 -1
                                         writeArray dup (y,x ) $ c1!(y,x)
                                         writeArray dup (y,x') $ c2!(y,x)
                           return dup
    where ((h0, w0), (h1, w1)) = bounds c1
          width  = (w1-w0+1)*2 + 4 + w0 - 1
