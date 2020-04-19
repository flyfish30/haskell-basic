{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, TypeApplications, RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}

module ComonadImg where

import qualified Codec.Picture        as Juicy
import qualified Codec.Picture.Types  as JuicyT
import           Data.List            (sort)
import           Data.Maybe           (fromMaybe, maybeToList)
import qualified Data.Vector          as V
import qualified Data.Vector.Generic  as VG
import           Data.Word

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
  (=>>) :: w a -> (w a -> b) -> w b
  (=>>) = flip extend

data BoxedImage a = BoxedImage
  { biWidth  :: !Int
  , biHeight :: !Int
  , biData   :: !(V.Vector a)
  }

instance Functor BoxedImage where
  fmap f (BoxedImage w h d) = BoxedImage w h (fmap f d)

type Pixel8 = Word8

boxImage :: Juicy.Image Juicy.Pixel8 -> BoxedImage Pixel8
boxImage image = BoxedImage
  { biWidth  = Juicy.imageWidth image
  , biHeight = Juicy.imageHeight image
  , biData   = VG.convert (Juicy.imageData image)
  }

unboxImage :: BoxedImage Pixel8 -> Juicy.Image Juicy.Pixel8
unboxImage boxedImage = Juicy.Image
  { Juicy.imageWidth  = biWidth boxedImage
  , Juicy.imageHeight = biHeight boxedImage
  , Juicy.imageData   = VG.convert (biData boxedImage)
  }

readImage :: FilePath -> IO (BoxedImage Pixel8)
readImage filePath = do
  errOrImage <- Juicy.readImage filePath
  case errOrImage of
    Right (Juicy.ImageY8 img) -> return (boxImage img)
    Right (Juicy.ImageYCbCr8 img) -> return (boxImage (JuicyT.extractLumaPlane img))
    Right (Juicy.ImageRGB8 img)   -> return (boxImage (JuicyT.extractLumaPlane img))
    Right (Juicy.ImageRGBA8 img)  -> return (boxImage (JuicyT.extractLumaPlane img))
    Right _                   -> error "readImage: unspported format"
    Left  err                 -> error $ "readImage: can not load image: " ++ err

writePng :: FilePath -> BoxedImage Pixel8 -> IO ()
writePng filePath = Juicy.writePng filePath . unboxImage

data FocusedImage a = FocusedImage 
  { boxedImage :: !(BoxedImage a)
  , cx         :: !Int
  , cy         :: !Int
  }

focus :: BoxedImage a -> FocusedImage a
focus bi
  | biWidth bi > 0 && biHeight bi > 0 = FocusedImage bi 0 0
  | otherwise                         = error "Can not focus on empty image"

unfocus :: FocusedImage a -> BoxedImage a
unfocus (FocusedImage bi _ _) = bi

instance Functor FocusedImage where
  fmap f (FocusedImage bi x y) = FocusedImage (fmap f bi) x y

instance Comonad FocusedImage where
  extract (FocusedImage bi x y) = (biData bi) V.! (biWidth bi * y + x)
  extend f (FocusedImage bi@(BoxedImage w h _) x y) = FocusedImage
      (BoxedImage w h $ V.generate (w * h) $ \i ->
          let (y', x') = i `quotRem` w
          in  f (FocusedImage bi x' y'))
      x y

neighbour :: Int -> Int -> FocusedImage a -> FocusedImage a
neighbour dx dy (FocusedImage bi x y) = FocusedImage bi x' y'
  where x' = wrap (x + dx) 0 (biWidth bi - 1)
        y' = wrap (y + dy) 0 (biHeight bi - 1)
        {-# INLINE wrap #-}
        wrap i lo hi
          | i < lo = lo - i
          | i > hi = hi - (i - hi)
          | otherwise = i

median :: Integral a => V.Vector a -> a 
median v = fromIntegral $ (sort $ V.toList v) !! (V.length v `quot` 2)

blur :: Integral a => V.Vector a -> a
blur v = fromIntegral
       $ (`quot` 16)
       $ ( nbi 0     + nbi 1 * 2 + nbi 2
         + nbi 3 * 2 + nbi 4 * 4 + nbi 5 * 2
         + nbi 6     + nbi 7 * 2 + nbi 8
         )
  where {-# INLINE nbi #-}
        nbi :: Int -> Word16
        nbi i = fromIntegral $ v V.! i

filterImage :: Integral a => (V.Vector a -> a) -> Int -> FocusedImage a -> a
filterImage filter kernelW fimg@(FocusedImage bi x y) = filter $
    V.generate kernelSize $ \i -> extract $ neighbour (nbx i) (nby i) fimg
  where nbx i = (-1) + (i `rem`  kernelW);
        nby i = (-1) + (i `quot` kernelW);
        {-# INLINE kernelSize #-}
        kernelSize = kernelW * kernelW

{-# INLINE medianImage #-}
medianImage :: Integral a => FocusedImage a -> a
medianImage = filterImage median 5

{-# INLINE blurImage #-}
blurImage :: Integral a => FocusedImage a -> a
blurImage = filterImage blur 3

reduceNoise :: Integral a => FocusedImage a -> a
reduceNoise fimg =
  let !original   = extract fimg
      !blurred    = blurImage fimg
      !edge       = fromIntegral original - fromIntegral blurred :: Int
      !threshold  = if edge < 7 && edge < (-7) then 0 else edge
  in fromIntegral $ fromIntegral blurred + threshold
  -- in fromIntegral threshold

reduceNoise1 :: Integral a => FocusedImage a -> a
reduceNoise1 fimg =
  let !original   = extract fimg
      !medianed   = medianImage fimg
      !reduced    = reduceNoise fimg
  in (original `quot` 4) + (medianed `quot` 4) + (reduced `quot` 2)

