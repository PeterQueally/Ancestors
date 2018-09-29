module LCA where 

data binTree b = Node b (binTree b) (binTree b) | Nil