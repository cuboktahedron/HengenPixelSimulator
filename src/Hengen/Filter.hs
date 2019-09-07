module Hengen.Filter
    ( identity
    , left
    , leftN
    , right
    , rightN
    , up
    , upN
    , down
    , downN
    )
where

import           Data.Bits
import           Hengen.Types


identity :: Canvas -> Canvas
identity ns = ns

right :: Canvas -> Canvas
right cv = rightN 1 cv

rightN :: Int -> Canvas -> Canvas
rightN n cv = map (\row -> row `shiftR` n) cv

left :: Canvas -> Canvas
left cv = leftN 1 cv

leftN :: Int -> Canvas -> Canvas
leftN n cv = map (\row -> row `shiftL` n) cv

up :: Canvas -> Canvas
up cv = upN 1 cv

upN :: Int -> Canvas -> Canvas
upN n cv = drop n cv ++ replicate n 0

down :: Canvas -> Canvas
down cv = downN 1 cv

downN :: Int -> Canvas -> Canvas
downN n cv = replicate n 0 ++ take (16 - n) cv
