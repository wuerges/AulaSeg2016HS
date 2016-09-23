 {-# LANGUAGE Rank2Types #-}

module Lib
    ( ceasar
    , ceasarKey
    , ceasarInverseKey
    , Cypher
    , substitution
    , subsInverseKey
    ) where

import Data.Word ( Word8 )
import Data.List ( sort )
import qualified Data.Map as M

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

subsInverseKey :: [Word8] -> [Word8]
subsInverseKey k = map fst $ sort $ zip k [0..]

substitution :: Cypher [Word8]
substitution k t = map lookup_k t
  where
    h = M.fromList (zip [0..] k)
    lookup_k = flip (M.findWithDefault 0) h
    



