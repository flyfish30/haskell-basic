module Main where

import qualified Data.Vector.Unboxed       as V

import           ComonadImg
import           SlideWindow
import           DiagramsGraph

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let count = 10000000
  let xs    = take count $ cycle [10..80]
  -- putStrLn $ show $ last (slideWindowSimple 5 sum xs :: [Int])
  -- putStrLn $ show $ last (slideWindowAccuR 5 (+) 0 xs :: [Int])
  -- putStrLn $ show $ last (slideWindowSemigrp 5 xs :: [Int])
  -- putStrLn $ show $ last (slideWindowGroup 5 xs :: [Int])
  -- putStrLn $ show $ last (slideWindowInt 5 xs :: [Int])
  -- putStrLn $ show $ V.last (slideWindowVSimple 5 V.sum $ V.fromList xs :: V.Vector Int)
  -- putStrLn $ show $ V.last (slideWindowVAccuR 5 (+) 0 $ V.fromList xs :: V.Vector Int)
  -- putStrLn $ show $ V.last (slideWindowVSemigrp 5 $ V.fromList xs :: V.Vector Int)
  -- putStrLn $ show $ V.last (slideWindowVGroup 5 $ V.fromList xs :: V.Vector Int)
  -- putStrLn $ show $ V.last (slideWindowVInt 5 $ V.fromList xs :: V.Vector Int)
  img <- readImage "data/yingwuhua.jpg"
  -- writePng "output.png" . unfocus . (=>> medianImage) . focus $ img
  writePng "output.png" . unfocus . (=>> blurImage) . focus $ img
  -- writePng "output.png" . unfocus . (=>> edgeImage) . focus $ img
  -- writePng "output.png" . unfocus . (=>> ebossImage) . focus $ img
  -- writePng "output.png" . unfocus . (=>> reduceNoise) . focus $ img
  -- img <- readUnboxImage "data/yingwuhua.jpg"
  -- putStrLn . show $ getHistogram img
  -- writeUnboxPng "output.png" $ blurBatchImage img
  -- writeUnboxPng "output.png" $ blurUbImage img
  -- drawExampleGraph
