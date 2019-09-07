module Hengen.Printer
  ( print
  )
where

import           Prelude                 hiding ( print )
import           Data.Bits

print :: [Int] -> [String]
print ns = map printLine ns

printLine :: Int -> String
printLine n = map printBit [15, 14 .. 0]
  where printBit p = if ((n `shiftR` p) .&. 1) == 1 then '1' else '0'
