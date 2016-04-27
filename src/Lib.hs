module Lib
    ( ceasar
    , ceasarSymbol
    , Symbol
    ) where

import Data.Word ( Word8 )

-- Functions related to Ceasar Cypher

type Symbol = Word8

ceasar :: Int -> [Symbol] -> [Symbol]
ceasar k s = map (ceasarSymbol k) s

ceasarSymbol :: Int -> Symbol -> Symbol
ceasarSymbol k s = fromIntegral $ si + k `mod` 256
  where si = (fromIntegral s) :: Int

