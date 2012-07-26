{-# LANGUAGE FlexibleContexts #-}
module NA.Image.Utils
    ( channelToDouble
    , channelToWord8
    , normalise
    , duplicate
    , toHSV
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

normalise :: (Fractional b, Ord b, IArray UArray b) => Image b -> Image b
normalise = imap normaliseChannel

normaliseChannel  :: (Fractional e, Ord e, Ix i, IArray a e) => a i e -> a i e
normaliseChannel c = let cMax = cfold1 max c
                         cMin = cfold1 min c
                     in amap (\x -> 255*(x-cMin)/(cMax-cMin)) c

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

toHSV :: (Integral e, IArray UArray e, IArray a e) => Image e -> (a Coordinate e, a Coordinate e, a Coordinate e)
toHSV (Image r g b) = let triplets = zip3 (elems r) (elems g) (elems b)
                          (h, s, v) = unzip3 $ map (getHSV 0) triplets
                          sizes = bounds r
                          mkArray = array sizes . zip (range sizes)
                      in (mkArray h, mkArray s, mkArray v)

getHSV :: Integral t => t -> (t, t, t) -> (t, t, t)
getHSV disp triplet@(x, y, z)
    | cMax == x = finishHSV triplet cMin disp
    | otherwise = getHSV (disp+85) (y, z, x)
    where cMax = maximum [x, y, z]
          cMin = minimum [x, y, z]

finishHSV :: Integral t => (t, t, t) -> t -> t -> (t, t, t)
finishHSV (x, y, z) cMin disp = let v = x
                                    s = x - cMin
                                    h = 43 * (y - z) `div` s + disp
                                in (h, s, v)
