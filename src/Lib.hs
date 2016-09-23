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
    , wordPattern
    , wordPatterns
    , countWordPatterns
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

count :: Ord a => [a] -> [(a, Int)]
count l = M.toList $ M.fromListWith (+) (zip l (repeat 1))

countSymbols :: [Word8] -> [(Word8, Int)]
countSymbols = count

calcEntropy :: [Word8] -> Double
calcEntropy t = -(sum $ map es $ countSymbols t)
  where
    es (_, f) = (fromIntegral f / fromIntegral (length t)) * logBase 256 (fromIntegral f / fromIntegral (length t))

-- Word Patterns
wordPattern :: [Word8] -> [Int]
wordPattern w = wordPatMap w 0 M.empty
  where
    wordPatMap [] _ _ = []
    wordPatMap (c:cs) n m = case M.lookup c m of
                              Nothing -> n:wordPatMap cs (n+1) (M.insert c n m)
                              Just v ->  v:wordPatMap cs n m

type Pattern = ([Int], [Word8])

wordPatterns :: Int -> [Word8] -> [Pattern]
wordPatterns _ [] = []
wordPatterns k t@(c:cs) = pats ++ wordPatterns k cs
  where prefs = map (flip take $ t) [1..(min k (length t))]
        pats = [(wordPattern p, p) | p <- prefs]


countWordPatterns :: [Pattern] -> [([Int], ([[Word8]], Int))]
countWordPatterns ps =
  M.toList $ M.fromListWith (\(w1, c1) (w2, c2) -> (w1 ++ w2, c1 + c2)) [(p, ([w], 1)) | (p, w) <- ps]



