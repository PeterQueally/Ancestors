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

functorTests = testGroup "Functor"
    [ testCase "fmap (*2) sampleTree produces doubled graph"
        (fmap (*2) sampleTree @?= sampleTreeDoubled)
    , QC.testProperty "fmapping inverse functions produces the input"
        fmapInversions
    ]
    
insertElementTests = testGroup "Node Insertion"
    [ testCase "Empty insertion"
        (insertElement Empty [GraphLeft, GraphRight] 3 @?= lastNode 3)
    , testCase "Leaf left insertion"
        (insertElement (lastNode 3) [GraphLeft] 4 @?= Node 3 (lastNode 4) Empty)
    , testCase "Leaf right insertion"
        (insertElement (lastNode 3) [GraphRight] 4 @?= Node 3 Empty (lastNode 4))
    , testCase "Intermediate Node insertion"
        (insertElement (Node 3 (lastNode 4) Empty) [GraphLeft] 5 @?=
            Node 3 (Node 5 (lastNode 4) Empty) Empty)
    , QC.testProperty "tree != insertElement <path> <el> tree"
        testInsertElementEq
    ]
    
pathFollowTests = testGroup "Path & Follow"
    [ testCase "Nonexistent path is Nothing"
        (paths sampleTreeDoubled 23456789 @?= [])
    , testCase "path to 451 in sampleTree"
        (paths sampleTree 451 @?= [[GraphLeft, GraphRight, GraphRight, GraphRight, GraphLeft]])
    , testCase "following path to 451 in sampleTree gives Just 451"
        (follow sampleTree (head $ paths sampleTree 451) @?= Just 451)
    , QC.testProperty "and ((== Just el) $ fmap (follow root) (path root el))"
        testFollowPath
    ]      

