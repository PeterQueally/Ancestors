module Main where

import Lib

main :: IO ()
main = do let tip n = Node n Nil Nil
          let tree = Node 8 (Node 3 (tip 1) (Node 6 (tip 4) (tip 7)))
                            (Node 10 Nil (Node 14 (tip 13) Nil))
