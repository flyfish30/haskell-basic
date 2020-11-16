{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, TypeApplications, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module DiagramsGraph where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Graphics.SVGFonts

drawExampleGraph :: IO ()
drawExampleGraph = mainWith row6node

myCircle :: Diagram B
myCircle = circle 1 # translate (r2 (0.6, 0.6)) # showOrigin

-- using SVG fonts
textOut t = stroke (textSVG t 0.6) # fc white

node :: Int -> Diagram B
node n = textOut (show n) <> circle 0.2 # fc green

row6node :: Diagram B
row6node = hcat . take 6 . map node $ [1..]
