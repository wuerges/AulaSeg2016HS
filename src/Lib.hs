module Lib
    ( ceasar
    , ceasarSymbol
    ) where

import Data.Word ( Word8 )

type Symbol = Word8

ceasar :: [Symbol] -> Int -> [Symbol]
ceasar s k = map (flip ceasarSymbol k) s

ceasarSymbol :: Symbol -> Int -> Symbol
ceasarSymbol s k = fromIntegral $ si + k `mod` 256
  where si = (fromIntegral s) :: Int
