module Main where

import           Control.Exception.Safe
import           Hengen.HengenPixel
import qualified Hengen.Lang.Graph as Graph
import           System.IO

main :: IO ()
main = do
  putStr "> " >> hFlush stdout
  file <- getLine
  if file == ""
    then return ()
    else do
      (Graph.execHengenPixel file)
        `catch` (\(HengenPixelException err) -> print err)
      putStrLn ""
      main
