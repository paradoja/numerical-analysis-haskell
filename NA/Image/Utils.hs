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

toHSV :: Image Word8 -> (Channel Word8, Channel Word8, Channel Word8)
toHSV img@(Image red green blue) = let channels = [red, green, blue]
                                       cMin :: Word8
                                       cMin     = minimum $ map (cfold1 min) channels
                                       cMaxs    = map (cfold1 max) channels
                                   in hsvMaxFirst cMaxs cMin img 0

hsvMaxFirst :: [Word8] -> Word8 -> Image Word8 -> Word8 -> (Channel Word8, Channel Word8, Channel Word8)
hsvMaxFirst list@[x,y,z] cMin img@(Image cx cy cz) disp
       | maximum list == x = finishHSV x cMin img disp
       | otherwise         = hsvMaxFirst [y,z,x] cMin (Image cy cz cx) (disp+85)
hsvMaxFirst _ _ _ _ = error "should receive a 3 element list"

finishHSV :: Word8 -> Word8 -> Image Word8 -> Word8 -> (Channel Word8, Channel Word8, Channel Word8)
finishHSV cMax cMin (Image cx cy cz) disp =
    let sizes = bounds cx
        diff = cMax - cMin
        v = array sizes $ zip (range sizes) $ repeat cMax
        s = array sizes $ zip (range sizes) $ repeat diff
        h = runSTUArray $ do
              new <- newArray sizes 0
              forM_ (reverse $ range sizes) $ \(j,i) -> do
                            let val = 43 *((cy!(j,i)) - (cz!(j,i))) `div` diff + disp
                            writeArray new (j,i ) val
              return new
        in (v, s, h)
