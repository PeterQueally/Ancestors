{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Lib

main = defaultMain tests

tests :: [TF.Test]
tests = [ testGroup "\n\nLowest common ancestor tests\n"
          [ ancestorTests
          ]
        ]

ancestorTests :: TF.Test   
ancestorTests
  = testGroup "Tests"
    [ testCase (lca emptyTree 1 2) @?= Left False
    ]  
            
{-  describe "Lca function" $ do

    it "returns Left False for empty tree" $ do
      (lca emptyTree 1 2) `shouldBe` Left False

    it "returns 1 for lca 1 1" $ do
      (lca testTree 1 1) `shouldBe` Right 1

    it "returns 1 for lca 2 3" $ do
          (lca testTree 2 3) `shouldBe` Right 1

    it "returns 2 for lca 4 9" $ do
      (lca testTree 4 9) `shouldBe` Right 2

    it "returns Left True for lca 4 100" $ do
      (lca testTree 4 100) `shouldBe` Left True

    it "returns Left False for lca 80 100" $ do
      (lca testTree 80 100) `shouldBe` Left False -}