module Hengen.HengenPixel where

import qualified Hengen.Lang.Mg as L
import qualified Hengen.Printer as P
import qualified Hengen.Scanner as S
import           Hengen.Types

data HGScanner = HGScanner [String] deriving Show

data HGFilter = HGFilter L.Program [HGNode] deriving Show

data HGPrinter = HGPrinter HGNode deriving Show

data HGNode = HGNFilter HGFilter
            | HGNScanner HGScanner
            | HGNEmpty deriving Show

test1 :: IO ()
test1 = printHG $ HGPrinter $ HGNScanner $ createScanner ""

test2 :: IO ()
test2 = printHG
  $ HGPrinter
  $ HGNFilter
  $ HGFilter (loadFilter "") [HGNScanner $ createScanner ""]

printHG :: HGPrinter -> IO ()
printHG (HGPrinter node) = mapM_ putStrLn $ P.print $ through node

through :: HGNode -> Canvas
through HGNEmpty = []
through (HGNFilter (HGFilter prog nodes)) =
  L.execProgram prog (map through nodes)
through (HGNScanner (HGScanner scanner)) = S.scan $ scanner

canvasF =
  [ "0000000000000000"
  , "0000000000000000"
  , "0011111111111100"
  , "0011111111111100"
  , "0011111111111100"
  , "0011100000000000"
  , "0011100000000000"
  , "0011111111100000"
  , "0011111111100000"
  , "0011111111100000"
  , "0011100000000000"
  , "0011100000000000"
  , "0011100000000000"
  , "0011100000000000"
  , "0000000000000000"
  , "0000000000000000"]

createScanner :: String -> HGScanner
createScanner _ = HGScanner canvasF

loadFilter :: FilePath -> L.Program
loadFilter path =
  let prog = L.parseProgram "\
      \program Program1\n\
      \  i <- 16;\n\
      \  while i do\n\
      \     n <- RECEIVE[0];\n\
      \     SEND ~n;\n\
      \     i <- i - 1;\n\
      \   end-while\n\
      \end-program"
  in case prog of
       (Left err)   -> error err
       (Right prog) -> prog
