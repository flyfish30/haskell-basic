module Main where

import System.Exit (ExitCode(..), exitFailure, exitSuccess, exitWith)
import ComonadImg

main :: IO ()
main = do
  putStrLn "Hello world!"
  img <- readImage "/home/SENSETIME/liuchangsheng/Pictures/webwxgeticon.jpeg"
  writePng "./output.png" .  (unfocus . (=>> reduceNoise1) . focus) $ img
  exitSuccess
