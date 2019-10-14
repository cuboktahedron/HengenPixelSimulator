module Main where

import qualified Hengen.Lang.Graph as Graph
import           System.IO

main :: IO ()
main = do
  putStr "> " >> hFlush stdout
  file <- getLine
  if file == ""
    then return ()
    else do
      Graph.execHengenPixel file
      main
