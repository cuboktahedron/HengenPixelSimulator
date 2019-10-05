module Main where

import           Control.Monad.State.Lazy
import qualified Hengen.Filter as F
import qualified Hengen.HengenPixel as HP
import qualified Hengen.Printer as P
import qualified Hengen.Scanner as S
import           Hengen.Types
import qualified Hengen.Lang.Mg as L

main :: IO ()

-- main = mapM_ putStrLn $ P.print $ F.identity $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.right $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.left $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.up $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.down $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.rightN 2 $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.leftN 2 $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.upN 2 $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.downN 2 $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.complement $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.swap $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.reverse $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.and [S.scan original, S.scan checker]
-- main = mapM_ putStrLn $ P.print $ F.or [S.scan original, S.scan checker]
-- main = mapM_ putStrLn $ P.print $ F.border $ S.scan original
--{ scanner { filter }* }+ -> Printer
main = do
  HP.test11

-- let scanner = HP.createScanner ""
--     filter = HP.loadFilter ""
-- in print $ scanner
original =
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

checker =
  [ "0101010101010101"
  , "1010101010101010"
  , "0101010101010101"
  , "1010101010101010"
  , "0101010101010101"
  , "1010101010101010"
  , "0101010101010101"
  , "1010101010101010"
  , "0101010101010101"
  , "1010101010101010"
  , "0101010101010101"
  , "1010101010101010"
  , "0101010101010101"
  , "1010101010101010"
  , "0101010101010101"
  , "1010101010101010"]
