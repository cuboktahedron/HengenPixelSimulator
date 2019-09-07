module Main where

import qualified Hengen.Filter as F
import qualified Hengen.Printer as P
import qualified Hengen.Scanner as S
import Hengen.Types

main :: IO ()
-- main = mapM_ putStrLn $ P.print $ F.identity $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.right $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.left $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.up $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.down $ S.scan original

-- main = mapM_ putStrLn $ P.print $ F.rightN 2 $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.leftN 2 $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.upN 2 $ S.scan original
main = mapM_ putStrLn $ P.print $ F.downN 2 $ S.scan original

original =
    [ "0000000000000000"
    , "0000000000000000"
    , "0001111111111100"
    , "0001111111111100"
    , "0001111111111100"
    , "0001110000000000"
    , "0001110000000000"
    , "0001111111100000"
    , "0001111111100000"
    , "0001111111100000"
    , "0001110000000000"
    , "0001110000000000"
    , "0001110000000000"
    , "0001110000000000"
    , "0000000000000000"
    , "0000000000000000"
    ]
