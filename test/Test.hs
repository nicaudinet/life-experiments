module Test where

import Test.Tasty
import Test.Tasty.QuickCheck

import Life.Evolve

main :: IO ()
main = defaultMain $ testGroup "Tests" [ testFitnessFunction ]

testFitnessFunction :: TestTree
testFitnessFunction = testGroup "Fitness function tests"
  [ propertySameGenome ]

propertySameGenome :: TestTree
propertySameGenome =
  testProperty "fitness genome (phenotype genome) == 0" $
    \genome -> fitness genome (phenotype genome) == 0
