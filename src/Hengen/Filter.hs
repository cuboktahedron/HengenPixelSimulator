{-# LANGUAGE BinaryLiterals #-}

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
        in  lToR .|. rToL

reverse :: Canvas -> Canvas
reverse = map reverseRow
  where
    reverseRow row =
        let m1 = 0b0101010101010101
            m2 = 0b0011001100110011
            m3 = 0b0000111100001111
            m4 = 0b0000000011111111
            r0 = row
            r1 = ((r0 .&. m1) `shiftL` 1) .|. ((r0 `shiftR` 1) .&. m1)
            r2 = ((r1 .&. m2) `shiftL` 2) .|. ((r1 `shiftR` 2) .&. m2)
            r3 = ((r2 .&. m3) `shiftL` 4) .|. ((r2 `shiftR` 4) .&. m3)
            r4 = ((r3 .&. m4) `shiftL` 8) .|. ((r3 `shiftR` 8) .&. m4)
        in  r4

and :: [Canvas] -> Canvas
and (cv : rest) = foldl (\acc cv -> acc `and'` cv) cv rest
  where
    and' cv1 cv2 = map andRow $ zip cv1 cv2
    andRow (row1, row2) = row1 .&. row2

or :: [Canvas] -> Canvas
or (cv : rest) = foldl (\acc cv -> acc `or'` cv) cv rest
  where
    or' cv1 cv2 = map orRow $ zip cv1 cv2
    orRow (row1, row2) = row1 .|. row2

border :: Canvas -> Canvas
border cv = Hengen.Filter.and
    [ cv
    , Hengen.Filter.complement $ Hengen.Filter.and
        [ Hengen.Filter.and [right cv, left cv]
        , Hengen.Filter.and [up cv, down cv]
        ]
    ]
