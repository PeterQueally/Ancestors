module Main where

import LCA

main :: IO ()
main = do let tip n = Node n Nil Nil
          let tree = Node 8 (Node 3 (tip 1) (Node 6 (tip 4) (tip 7)))
                            (Node 10 Nil (Node 14 (tip 13) Nil))
         print $ lca 4  7 tree == 6
         print $ lca 4 10 tree == 8
         print $ lca 1  4 tree == 3
         print $ lca 1  3 tree == 3
         print $ lca 3  6 tree == 3
