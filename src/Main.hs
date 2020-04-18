module Main where

import ComonadImg

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  img <- readImage "data/yingwuhua.jpg"
  -- writePng "output.png" . unfocus . (=>> medianImage) . focus $ img
  -- writePng "output.png" . unfocus . (=>> blurImage) . focus $ img
  writePng "output.png" . unfocus . (=>> reduceNoise1) . focus $ img