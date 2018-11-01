module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Graph

main :: IO ()
main = defaultMain tests

constructorTests = testGroup "Constructors"
    [ testCase "Empty == Empty" (Empty @=? (Empty :: Graph Int))
    , QC.testProperty "lastNode a == Node a Empty Empty" testLastNode
    , testCase "sampleTree converts to correct string"
        (show sampleTree @?=
            "Node 0 (Node 3 Empty (Node 5 (Node 100 Empty (Node 1 " ++
            "Empty Empty)) (Node 7 Empty (Node 42 (Node 451 Empty " ++
            "Empty) Empty)))) (Node 8 Empty (Node 43 (Node 500 Empty " ++
            "Empty) Empty))")
    , testCase "treeFromList [0..5]"
        (treeFromList [0..5] @?= treeToFive)
    , testCase "treeFromList [] == Empty"
        (treeFromList [] @?= (Empty :: Graph Int))
    ]
      

