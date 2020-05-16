{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, TypeApplications, RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}

module ComonadImg where

import qualified Codec.Picture        as Juicy
import qualified Codec.Picture.Types  as JuicyT
import           Data.List            (sort)
import           Data.Maybe           (fromMaybe, maybeToList)
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as VU
import qualified Data.Vector.Mutable  as MV
import qualified Data.Vector.Generic  as VG
import qualified Data.Vector.Algorithms.Merge  as VMerge
import           Data.Word

import Debug.Trace

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
  fmap :: (a -> b) -> BoxedImage a -> BoxedImage b
  fmap f (BoxedImage w h d) = BoxedImage w h (V.map f d)

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
  extract :: FocusedImage a -> a
  extract (FocusedImage bi x y) = (biData bi) V.! (biWidth bi * y + x)

  extend :: (FocusedImage a -> b) -> FocusedImage a -> FocusedImage b
  extend f (FocusedImage bi@(BoxedImage w h _) x y) = FocusedImage
      (BoxedImage w h $ V.generate (w * h) $ \i ->
          let (y', x') = i `quotRem` w
          in  f (FocusedImage bi x' y'))
      x y

neighbour :: Int -> Int -> FocusedImage a -> a
neighbour dx dy (FocusedImage bi x y) = (biData bi) V.! (biWidth bi * y' + x')
  where x' = wrap (x + dx) 0 (biWidth bi - 1)
        y' = wrap (y + dy) 0 (biHeight bi - 1)
        {-# INLINE wrap #-}
        wrap i lo hi
          | i < lo = lo - i
          | i > hi = hi - (i - hi)
          | otherwise = i

median :: Integral a => V.Vector a -> a 
median v = fromIntegral $ (V.modify VMerge.sort v) V.! (V.length v `quot` 2)

blur :: Integral a => V.Vector a -> a
blur v = fromIntegral
       $ (`quot` 16)
       $ ( nbi 0     + nbi 1 * 2 + nbi 2
         + nbi 3 * 2 + nbi 4 * 4 + nbi 5 * 2
         + nbi 6     + nbi 7 * 2 + nbi 8
         )
  where {-# INLINE nbi #-}
        nbi :: Int -> Word32
        nbi i = fromIntegral $ v V.! i

edge :: Integral a => V.Vector a -> a
edge v = fromIntegral
       $ (+ 127)
       $ ( nbi 0 * (-1) + 0         + nbi 2 * (-1)
         + 0            + nbi 4 * 4 + 0
         + nbi 6 * (-1) + 0         + nbi 8 * (-1)
         )
  where {-# INLINE nbi #-}
        nbi :: Int -> Int
        nbi i = fromIntegral $ v V.! i

eboss :: Integral a => V.Vector a -> a
eboss v = fromIntegral
       $ (+ 127)
       $ ( nbi 0 * 3 `quot` 2 + 0  + 0
         + 0                  + 0  + 0
         + 0                  + 0  + nbi 8 * (-3) `div` 2
         )
  where {-# INLINE nbi #-}
        nbi :: Int -> Int
        nbi i = fromIntegral $ v V.! i

filterImage :: Integral a => (V.Vector a -> a) -> Int -> FocusedImage a -> a
filterImage filter kernelW fimg@(FocusedImage bi x y) = filter $
    V.generate kernelSize $ \i -> neighbour (nbx i) (nby i) fimg
  where nbx i = (-(kernelW `quot` 2)) + (i `rem`  kernelW);
        nby i = (-(kernelW `quot` 2)) + (i `quot` kernelW);
        {-# INLINE kernelSize #-}
        kernelSize = kernelW * kernelW

{-# INLINE medianImage #-}
medianImage :: Integral a => FocusedImage a -> a
medianImage = filterImage median 5

{-# INLINE blurImage #-}
blurImage :: Integral a => FocusedImage a -> a
blurImage = filterImage blur 3

{-# INLINE edgeImage #-}
edgeImage :: Integral a => FocusedImage a -> a
edgeImage = filterImage edge 3

{-# INLINE ebossImage #-}
ebossImage :: Integral a => FocusedImage a -> a
ebossImage = filterImage eboss 3

reduceNoise :: Integral a => FocusedImage a -> a
reduceNoise fimg =
  let !original   = extract fimg
      !blurred    = blurImage fimg
      !edged       = fromIntegral original - fromIntegral blurred :: Int
      !threshold  = if edged < 7 && edged < (-7) then 0 else edged
  in fromIntegral $ fromIntegral blurred + threshold
  -- in fromIntegral threshold

reduceNoise1 :: Integral a => FocusedImage a -> a
reduceNoise1 fimg =
  let !original   = extract fimg
      !medianed   = medianImage fimg
      !reduced    = reduceNoise fimg
  in (original `quot` 4) + (medianed `quot` 4) + (reduced `quot` 2)

-- Unboxed vector image
data UnboxedImage a = UnboxedImage
  { ubWidth  :: !Int
  , ubHeight :: !Int
  , ubData   :: !(VU.Vector a)
  }

ubImgFromJuicy :: Juicy.Image Juicy.Pixel8 -> UnboxedImage Pixel8
ubImgFromJuicy image = UnboxedImage
  { ubWidth  = Juicy.imageWidth image
  , ubHeight = Juicy.imageHeight image
  , ubData   = VG.convert (Juicy.imageData image)
  }

ubImgToJuicy :: UnboxedImage Pixel8 -> Juicy.Image Juicy.Pixel8
ubImgToJuicy unboxedImage = Juicy.Image
  { Juicy.imageWidth  = ubWidth unboxedImage
  , Juicy.imageHeight = ubHeight unboxedImage
  , Juicy.imageData   = VG.convert (ubData unboxedImage)
  }

readUnboxImage :: FilePath -> IO (UnboxedImage Pixel8)
readUnboxImage filePath = do
  errOrImage <- Juicy.readImage filePath
  case errOrImage of
    Right (Juicy.ImageY8 img) -> return (ubImgFromJuicy img)
    Right (Juicy.ImageYCbCr8 img) -> return (ubImgFromJuicy (JuicyT.extractLumaPlane img))
    Right (Juicy.ImageRGB8 img)   -> return (ubImgFromJuicy (JuicyT.extractLumaPlane img))
    Right (Juicy.ImageRGBA8 img)  -> return (ubImgFromJuicy (JuicyT.extractLumaPlane img))
    Right _                   -> error "readImage: unspported format"
    Left  err                 -> error $ "readImage: can not load image: " ++ err

writeUnboxPng :: FilePath -> UnboxedImage Pixel8 -> IO ()
writeUnboxPng filePath = Juicy.writePng filePath . ubImgToJuicy

{-# INLINE chunkLen #-}
chunkLen = 256

{-# INLINE batchW #-}
batchW = 8

{-# INLINE batchH #-}
batchH = 1

getHistogram :: UnboxedImage Pixel8 -> VU.Vector Word32
getHistogram ubImage = go 0 vec vhist
  where n   = VU.length vec
        vec = (ubData ubImage)
        vhist = VU.replicate 256 0
        ones  = VU.replicate chunkLen 1
        zipVecOnes vs = (VU.zip (VU.map fromIntegral vs) ones)
        go !i !tvec !hist
          | i < n - chunkLen = go (i + chunkLen) (VU.drop chunkLen tvec)
                                  (VU.accumulate (+) hist
                                                     (zipVecOnes $ VU.take chunkLen tvec))
          | otherwise = VU.accumulate (+) hist (zipVecOnes $ VU.take (n - i) tvec)

filterUbVecImage :: VU.Unbox a => (V.Vector (VU.Vector a) -> (VU.Vector a))
                            -> Int
                            -> UnboxedImage a
                            -> UnboxedImage a
filterUbVecImage filter kernelW ubimg@(UnboxedImage w h ub) = UnboxedImage {
    ubWidth  = w
  , ubHeight = h
  , ubData   = V.foldl1 (VU.++) $ V.generate (w' * h) $ \idx -> 
                    -- trace (show idx) $
                    filter $
                    V.generate kernelSize $ \i ->
                        neighbourUbVec (getBlockxy idx) ((nbx i), (nby i)) ubimg
  }
  where nbx i = (-(kernelW `quot` 2)) + (i `rem`  kernelW);
        nby i = (-(kernelW `quot` 2)) + (i `quot` kernelW);
        w' = (w + batchW - 1) `quot` batchW
        getBlockxy idx = uncurry (flip (,)) $ idx `quotRem` w'
        {-# INLINE kernelSize #-}
        kernelSize = kernelW * kernelW


neighbourUbVec :: VU.Unbox a => (Int, Int) -> (Int, Int) -> UnboxedImage a -> VU.Vector a
neighbourUbVec (blockx, blocky) (dx, dy) (UnboxedImage w h ub)
  | sx < 0       = VU.reverse (VU.slice woff wlx ub)
                 VU.++ VU.slice off lx ub
  | ex > (w - 1) = VU.slice off lx ub
                 VU.++ VU.reverse (VU.slice woff wlx ub)
  | otherwise    = VU.slice off lx ub
  where x = blockx * batchW
        y = blocky
        sx = x + dx
        ex = sx + batchW - 1
        sy = y + dy
        ey = sy + batchH - 1
        lx = batchW - wlx
        (wsx, wlx) = wrapx
        ly = batchH
        off  = wsy * w + max 0 sx
        woff = wsy * w + wsx
        wrapx
          | sx < 0       = (1, 0 - sx)
          | ex > (w - 1) = (w - 1 - (ex - (w - 1)), (ex - (w - 1)))
          | otherwise    = (sx, 0)
        (wsy, wly) = wrapy
        wrapy
          | sy < 0       = (0 - sy, 0 - sy)
          | ey > (h - 1) = (h - 1 - (ey - (h - 1)), (ey - (h - 1)))
          | otherwise    = (sy, 0)

instance (Num a, VU.Unbox a) => Num (VU.Vector a) where
  fromInteger = VU.replicate batchW . fromInteger
  (+) = VU.zipWith (+)
  (-) = VU.zipWith (-)
  (*) = VU.zipWith (*)
  negate = VU.map negate
  abs    = VU.map abs
  signum = VU.map signum

notImplement :: String -> a
notImplement = error . (++ " is not implemented")

blurUbVec :: (Integral a, VU.Unbox a) => V.Vector (VU.Vector a) -> VU.Vector a
blurUbVec vvec = VU.map fromIntegral
               $ VU.map (`quot` 16)
               $ ( nbi 0     + nbi 1 * 2 + nbi 2
                 + nbi 3 * 2 + nbi 4 * 4 + nbi 5 * 2
                 + nbi 6     + nbi 7 * 2 + nbi 8
                 )
  where {-# INLINE nbi #-}
        nbi :: Int -> VU.Vector Word32
        nbi i = VU.map fromIntegral $ vvec V.! i

{-# INLINE blurUbVecImage #-}
blurUbVecImage :: (VU.Unbox a, Integral a) => UnboxedImage a -> UnboxedImage a
blurUbVecImage = filterUbVecImage blurUbVec 3

