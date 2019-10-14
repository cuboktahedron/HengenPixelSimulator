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

printHG :: HGNode -> IO ()
printHG (HGNPrinter (HGPrinter node)) = mapM_ putStrLn $ P.print $ through node

through :: HGNode -> Canvas
through HGNEmpty = []
through (HGNFilter (HGFilter prog nodes)) =
  L.execProgram prog (map through nodes)
through (HGNScanner (HGScanner scanner)) = S.scan $ scanner
through (HGNPrinter (HGPrinter node)) = through node

createScannerIO :: FilePath -> IO HGScanner
createScannerIO file = do
  let parent = parseRelDir $ "./data/scanner" :: IO (Path Rel Dir)
      f = parseRelFile $ file :: IO (Path Rel File)
      sf = (</>) <$> parent <*> f
  path <- toFilePath <$> sf
  cs <- readFile path
  let canvas = take 16 $ lines cs
  return (HGScanner canvas)

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