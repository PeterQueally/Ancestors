module LCA where 

data binTree b = Node b (binTree b) (binTree b) | Nil

lca :: Ord a => a -> a -> BinTree a -> a