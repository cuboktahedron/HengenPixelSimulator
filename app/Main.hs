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
-- main = mapM_ putStrLn $ P.print $ F.downN 2 $ S.scan original

-- main = mapM_ putStrLn $ P.print $ F.complement $ S.scan original
-- main = mapM_ putStrLn $ P.print $ F.swap $ S.scan original
main = mapM_ putStrLn $ P.print $ F.reverse $ S.scan original


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
    , "0000000000000000"
    ]
