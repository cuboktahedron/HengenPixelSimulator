module Hengen.Filter where

import           Data.Bits
import           Hengen.Types


identity :: Canvas -> Canvas
identity ns = ns

right :: Canvas -> Canvas
right = rightN 1

rightN :: Int -> Canvas -> Canvas
rightN n = map (\row -> row `shiftR` n)

left :: Canvas -> Canvas
left = leftN 1

leftN :: Int -> Canvas -> Canvas
leftN n = map (\row -> row `shiftL` n)

up :: Canvas -> Canvas
up = upN 1

upN :: Int -> Canvas -> Canvas
upN n cv = drop n cv ++ replicate n 0

down :: Canvas -> Canvas
down = downN 1

downN :: Int -> Canvas -> Canvas
downN n cv = replicate n 0 ++ take (16 - n) cv

complement :: Canvas -> Canvas
complement = map (\row -> row `xor` 0xffff)

swap :: Canvas -> Canvas
swap = map swapRow
  where
    swapRow row =
        let lToR = row `shiftR` 8
            rToL = row `shiftL` 8
        in lToR .|. rToL
