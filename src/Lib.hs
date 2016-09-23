 {-# LANGUAGE Rank2Types #-}

module Lib
    ( ceasar
    , ceasarKey
    , ceasarInverseKey
    , Cypher
    , substitution
    , subsInverseKey
    , calcEntropy
    , countSymbols
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

-- Functions related to Substitution Cipher

subsInverseKey :: [Word8] -> [Word8]
subsInverseKey k = map snd $ sort $ zip k [0..]

substitution :: Cypher [Word8]
substitution k t = map lookup_k t
  where
    h = M.fromList (zip [0..] k)
    lookup_k = flip (M.findWithDefault 0) h
    

-- Frequency calculation

countSymbols :: [Word8] -> M.Map Word8 Int
countSymbols t = M.fromListWith (+) (zip t (repeat 1))

calcEntropy :: [Word8] -> Double
calcEntropy t = (-1) * (sum $ map es $ M.toList (countSymbols t))
  where
    es (_, f) = (fromIntegral f / fromIntegral (length t)) * logBase 256 (fromIntegral f / fromIntegral (length t))
    
