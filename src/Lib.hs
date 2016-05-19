 {-# LANGUAGE Rank2Types #-}

module Lib
    ( ceasar
    , ceasarSymbol
    , Symbol
    ) where

import Data.Word ( Word8 )

-- Functions related to Ceasar Cypher


class Symbol a where
  (.+.) :: a -> Int -> a
  (.-.) :: a -> Int -> a
  v1 .-. v2 = v1 .+. (-v2)

type Cypher a k = Symbol a => k -> [a] -> [a]

instance Symbol Word8 where
  s .+. k = fromIntegral $ si + k `mod` 256
    where si = (fromIntegral s) :: Int



ceasar :: Cypher a Int
--ceasar :: Symbol a => Int -> [a] -> [a]
ceasar k s = map (ceasarSymbol k) s

ceasarSymbol :: Symbol a => Int -> a -> a
ceasarSymbol k s = s .+. k

