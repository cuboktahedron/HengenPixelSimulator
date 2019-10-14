module Main where

import qualified Hengen.Lang.Graph as Graph

main :: IO ()

main = do
  test1
  test2

test1 = Graph.execHengenPixel "minimal.dat"
test2 = Graph.execHengenPixel "border.dat"
  