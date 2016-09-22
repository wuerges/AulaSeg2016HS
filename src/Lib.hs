 {-# LANGUAGE Rank2Types #-}

module Lib
    ( ceasar
    , ceasarKey
    , ceasarInverseKey
    , Cypher
    ) where

import Data.Word ( Word8 )

-- Functions related to Ceasar Cypher

type Cypher a = a -> [Word8] -> [Word8]

(.+.) :: Word8 -> Int -> Word8
s .+. k = fromIntegral $ si + k `mod` 256
  where si = (fromIntegral s) :: Int



ceasarKey :: String -> Int
ceasarKey = read

ceasarInverseKey :: Int -> Int
ceasarInverseKey = negate

ceasar :: Cypher Int
ceasar k s = map (ceasarSymbol k) s

ceasarSymbol :: Int -> Word8 -> Word8
ceasarSymbol k s = s .+. k

