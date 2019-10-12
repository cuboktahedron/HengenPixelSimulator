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

data HGNode = HGNPrinter HGPrinter
            | HGNFilter HGFilter
            | HGNScanner HGScanner
            | HGNEmpty
  deriving Show

test11 :: IO ()
test11 = do
  f <- createScannerIO "F.dat"
  and <- loadFilterIO "And.dat"
  up <- loadFilterIO "Up.dat"
  down <- loadFilterIO "Down.dat"
  left <- loadFilterIO "Left.dat"
  right <- loadFilterIO "Right.dat"
  complement <- loadFilterIO "Complement.dat"

  let s1 = HGNScanner f
      f1 = HGNFilter $ HGFilter and [s1, f2]
      f2 = HGNFilter $ HGFilter complement [f3]
      f3 = HGNFilter $ HGFilter and [f4, f5]
      f4 = HGNFilter $ HGFilter and [f6, f7]
      f5 = HGNFilter $ HGFilter and [f8, f9]
      f6 = HGNFilter $ HGFilter right [s1]
      f7 = HGNFilter $ HGFilter left [s1]
      f8 = HGNFilter $ HGFilter up [s1]
      f9 = HGNFilter $ HGFilter down [s1]
      p  = HGNPrinter $ HGPrinter $ f1
  
  printHG p

printHG :: HGNode -> IO ()
printHG (HGNPrinter (HGPrinter node)) = mapM_ putStrLn $ P.print $ through node

through :: HGNode -> Canvas
through HGNEmpty = []
through (HGNFilter (HGFilter prog nodes)) =
  L.execProgram prog (map through nodes)
through (HGNScanner (HGScanner scanner)) = S.scan $ scanner
through (HGNPrinter (HGPrinter node)) = through node

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