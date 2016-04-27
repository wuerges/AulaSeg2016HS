module Lib
    ( someFunc
    , ceasar
    ) where

import Data.Word ( Word8 )


someFunc :: IO ()
someFunc = putStrLn "someFunc"


type Symbol = Word8


ceasar :: Symbol -> Int -> Symbol
ceasar s i = fromIntegral $ si + i `mod` 256
  where si = (fromIntegral s) :: Int
