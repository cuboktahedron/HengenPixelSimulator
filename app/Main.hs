module Main where

import qualified Hengen.Lang.Graph as Graph

main :: IO ()

main = do
  test3

test1 = Graph.execHengenPixel "minimal.dat"
test2 = Graph.execHengenPixel "border.dat"
test3 = Graph.execHengenPixel "swap_hv.dat"

  