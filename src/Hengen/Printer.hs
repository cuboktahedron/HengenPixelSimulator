module Hengen.Printer (Hengen.Printer.print) where

import           Data.Bits
import           Hengen.Types

print :: Canvas -> [String]
print ns = map printRow ns

printRow :: CanvasRow -> String
printRow n = map printBit [15, 14 .. 0]
  where
    printBit p = if ((n `shiftR` p) .&. 1) == 1
                 then '1'
                 else '0'
