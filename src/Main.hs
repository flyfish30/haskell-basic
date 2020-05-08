module Main where

import ComonadImg
import SlideWindow

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let count = 1000000
  -- putStrLn $ show $ last (slideWindowSimple 5 sum (replicate count 20) :: [Int])
  -- putStrLn $ show $ last (slideWindowAccuR 5 (+) 0 (replicate count 20) :: [Int])
  putStrLn $ show $ last (slideWindowSemigrp 5 (replicate count 20) :: [Int])
  -- img <- readImage "data/yingwuhua.jpg"
  -- writePng "output.png" . unfocus . (=>> medianImage) . focus $ img
  -- writePng "output.png" . unfocus . (=>> blurImage) . focus $ img
  -- writePng "output.png" . unfocus . (=>> edgeImage) . focus $ img
  -- writePng "output.png" . unfocus . (=>> ebossImage) . focus $ img
  -- writePng "output.png" . unfocus . (=>> reduceNoise) . focus $ img
