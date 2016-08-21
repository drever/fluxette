import Test.Hspec
import Test.QuickCheck

import Model

testsolutions :: [(Card, Card, Card)]
testsolutions = [ (Card Red One Diamond Full, Card Green Two Diamond Full, Card Blue Three Diamond Full)
                , (Card Blue One Diamond Half, Card Blue One Circle Empty, Card Blue One Box Full)]

testNonSolutions :: [(Card, Card, Card)]
testNonSolutions = [ (Card Red One Diamond Full, Card Red One Box Empty, Card Green Two Box Empty),
                     (Card Green One Diamond Full, Card Green One Diamond Empty, Card Green Two Diamond Empty) ]


main :: IO ()
main = hspec $ do
  describe "solutionCards" $ do
    it ("This has has two solutions: " ++ show testsolutions) $ do
      length (filter isSolution testsolutions) `shouldBe` 2
    it ("This has no solution: " ++ show testNonSolutions) $ do
       length (filter isSolution testNonSolutions) `shouldBe` 0
