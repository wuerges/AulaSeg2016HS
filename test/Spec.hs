import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Lib

main :: IO ()
main = do
  defaultMain tests

tests =
  [ makeCypherTestGroup "\n ceasarSymbol" ceasarSymbol
  , makeCypherTestGroup "\n ceasar" ceasar
  ]

testNegativeKey cypher s k   = cypher (cypher s (-k)) k      == s
testMultipleKey cypher n s k = cypher s (n * 256 + k)        == cypher s k
testComplementKey cypher s k = cypher (cypher s k) (256 - k) == s

makeCypherTestGroup name cypher =
  testGroup name [ testProperty "negative" (testNegativeKey cypher)
                 , testProperty "multiple" (testMultipleKey cypher)
                 , testProperty "complement" (testComplementKey cypher)
                 ]
