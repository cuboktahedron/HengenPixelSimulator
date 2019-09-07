module Hengen.Scanner
    ( scan
    )
where

import           Data.Bits

scan :: [String] -> [Int]
scan css = map scanLine css

scanLine :: String -> Int
scanLine cs = sum $ map scanBit [0 .. 15]
  where
    scanBit n =
        let bit = [cs !! n]
            v   = read bit
        in  v `shiftL` (15 - n)


