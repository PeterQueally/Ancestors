module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)


import LCA
{- Hunit Tests -}
main = defaultMain tests
-- Test to see if the BST can be created in a normal case
tests :: [TF.Test]
tests = [ testGroup "LCA Tests"
          [simpleTests
          ]
        ]
      
ts = fromList [9,8,7]        
us = fromList [6,4,2,1]
ds = fromList [7,5,3,2,1]
zs = fromList [8,4,2,1]
emptyls = fromList []

simpleTests:: TF.Test
simpleTests
 = testGroup "\nPart 1 simple\n"
  [ testCase "test length of 4"
    (size (fromList [1,2,3,4]) @?= 4),
    testCase "test length of 1"
    (size (fromList [5]) @?= 1),
    testCase "size of empty lists"
    (size (emptyls) @?= 0),
    testCase "empty path"
    (toList  (lca us emptyls) @?= []),
    testCase "both empty and same path"
    (toList (lca emptyls emptyls) @?= []),
    testCase "lca of two normal paths of diffrent lengths"
    (toList (lca us ds) @?= [2,1]),
    testCase "lca of two normal paths of same lengths"
    (toList (lca' us zs) @?= [4,2,1]),
    testCase "testing keep of 0 of two paths"
   (toList(keep 0 (lca xs ys)) @?= []),
   testCase "testing keep of 2"
   (toList(keep 2 (lca xs ys)) @?= [2,1]),
   testCase "lca with no match"
   (toList(lca xs ts) @?= []),
   testCase "lca of the same paths"
   (lca xs xs @?= xs)

  ]

-- BSTtest = BST (1,2)
-- BSTtest = BST (BSTtest,3)
-- Tests = test [ "test1" ~: "for (BST create 1)" ~: (True) ~=? (bstContains (BSTtest , 1) ),
--               "test2" ~: "for (BST create 2)" ~: (True) ~=? (bstContains (BSTtest , 2) ),
--               "test3" ~: "for (BST create 3)" ~: (True) ~=? (bstContains (BSTtest , 3) )]

{-- Test to see if BST return correct error for a empty tree
test3 :: Test
Test3 = testCase (assertBool "for (BST)")
-- Test if bstContains can correctly find a value in a given BST
test2 :: Test
Test2 = testCase (assertBool "for (bstContains)")
-- Test if bstContains will return false correctly
test3 :: Test
Test3 = testCase (assertEqual "for (bstContains)")
-}
