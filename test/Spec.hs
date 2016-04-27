import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Lib

main :: IO ()
main = do
  defaultMain tests

tests = [
  testGroup "\nTest Ceasar for Symbol" [
    testProperty "prop1" testCeasarSymbolWithNegativeKey,
    testProperty "prop2" testCeasarSymbolWithMultipleKey,
    testProperty "prop3" testCeasarSymbolWithComplementKey
                                     ]
        ]

testCeasarSymbolWithNegativeKey s k = ceasar (ceasar s (-k)) k == s
testCeasarSymbolWithMultipleKey n s k = ceasar s (n * 256 + k) == ceasar s k
testCeasarSymbolWithComplementKey s k = ceasar (ceasar s k) (256 - k) == s
