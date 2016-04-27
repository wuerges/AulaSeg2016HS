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

testNegativeKey cypher k s   = cypher k (cypher (-k) s)      == s
testMultipleKey cypher n k s = cypher (n * 256 + k) s        == cypher k s
testComplementKey cypher k s = cypher (256 - k) (cypher k s) == s

makeCypherTestGroup name cypher =
  testGroup name [ testProperty "negative" (testNegativeKey cypher)
                 , testProperty "multiple" (testMultipleKey cypher)
                 , testProperty "complement" (testComplementKey cypher)
                 ]
