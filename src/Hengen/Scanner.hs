module Hengen.Scanner
    ( scan
    )
where

import           Data.Bits
import           Hengen.Types

scan :: [String] -> Canvas
scan css = map scanLine css

scanLine :: String -> CanvasRow
scanLine cs = sum $ map scanBit [0 .. 15]
  where
    scanBit n =
        let bit = [cs !! n]
            v   = read bit
        in  v `shiftL` (15 - n)
