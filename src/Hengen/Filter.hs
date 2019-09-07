module Hengen.Filter
    ( identity
    , left
    , right
    , up
    , down
    )
where

import           Data.Bits
import           Hengen.Types


identity :: Canvas -> Canvas
identity ns = ns

right :: Canvas -> Canvas
right cv = map (\row -> row `shiftR` 1) cv

left :: Canvas -> Canvas
left cv = map (\row -> row `shiftL` 1) cv

up :: Canvas -> Canvas
up (row : rest) = rest ++ [0]

down :: Canvas -> Canvas
down (rest) = 0 : take 15 rest
