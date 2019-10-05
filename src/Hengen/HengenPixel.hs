module Hengen.HengenPixel where

import qualified Hengen.Lang.Mg as L
import qualified Hengen.Printer as P
import qualified Hengen.Scanner as S
import           Hengen.Types
import           Path
import           Path.IO

data HGScanner = HGScanner [String]
  deriving Show

data HGFilter = HGFilter L.Program [HGNode]
  deriving Show

data HGPrinter = HGPrinter HGNode
  deriving Show

data HGNode = HGNFilter HGFilter
            | HGNScanner HGScanner
            | HGNEmpty
  deriving Show

test1 :: IO ()
test1 = printHG $ HGPrinter $ HGNScanner $ createScanner "F"

test2 :: IO ()
test2 = printHG
  $ HGPrinter
  $ HGNFilter
  $ HGFilter (loadFilter "") [HGNScanner $ createScanner "F"]

test3 :: IO ()
test3 = do
  f <- createScannerIO "F.dat"
  complement <- loadFilterIO "Complement.dat"
  printHG $ HGPrinter $ HGNFilter $ HGFilter complement [HGNScanner f]
  return ()

test4 :: IO ()
test4 = do
  f <- createScannerIO "F.dat"
  left <- loadFilterIO "Left.dat"
  printHG $ HGPrinter $ HGNFilter $ HGFilter left [HGNScanner f]
  return ()

test5 :: IO ()
test5 = do
  f <- createScannerIO "F.dat"
  right <- loadFilterIO "Right.dat"
  printHG $ HGPrinter $ HGNFilter $ HGFilter right [HGNScanner f]
  return ()

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

createScannerIO :: FilePath -> IO HGScanner
createScannerIO file = do
  let parent = parseRelDir $ "./data/scanner" :: IO (Path Rel Dir)
      f = parseRelFile $ file :: IO (Path Rel File)
      sf = (</>) <$> parent <*> f
  path <- toFilePath <$> sf
  cs <- readFile path
  let canvas = take 16 $ lines cs
  return (HGScanner canvas)

loadFilter :: FilePath -> L.Program
loadFilter path =
  let prog = L.parseProgram
        "\
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

loadFilterIO :: FilePath -> IO L.Program
loadFilterIO file = do
  let parent = parseRelDir $ "./data/filter" :: IO (Path Rel Dir)
      f = parseRelFile $ file :: IO (Path Rel File)
      sf = (</>) <$> parent <*> f
  path <- toFilePath <$> sf
  cs <- readFile path
  let prog = L.parseProgram cs
  case prog of
    (Left err)   -> error err
    (Right prog) -> return prog