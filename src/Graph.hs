module Graph where

import Data.List 
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

data Graph a = Empty
             | Blank (Graph a) (Graph a)
             | Node a (Graph a) (Graph a)
               deriving (Eq, Ord, Show)

data GraphDir = GraphLeft
             | GraphRight
               deriving (Eq, Ord, Show)

newtype FullGraph a = FullGraph [Graph a] deriving (Eq, Ord, Show)

type GraphPath = [GraphDir]

lastNode :: a -> Graph a
lastNode a = Node a Empty Empty

instance Functor Graph where
    fmap fun Empty = Empty
    fmap fun (Blank l r) = Blank (fmap fun l) (fmap fun r)
    fmap fun (Node x l r) = Node (fun x) (fmap fun l) (fmap fun r)

paths :: Eq a => Graph a -> a -> [GraphPath]
paths Empty _ = []
paths n el = reverse <$> pathsInt n el []
    
pathsInt :: Eq a => Graph a -> a -> GraphPath -> [GraphPath]
pathsInt (Blank l r) el p = amassPaths l r el p
pathsInt (Node a l r) el p =
        (if a == el then (p:) else id) $ amassPaths l r el p
pathsInt _ _ _ = []