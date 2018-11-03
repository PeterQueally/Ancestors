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

amassPaths :: Eq a => Graph a -> Graph a -> a -> GraphPath -> [GraphPath]
amassPaths l r el p =
    let left  = pathsInt l el (GraphLeft  : p)
        right = pathsInt r el (GraphRight : p)
    in left ++ right

nodesAlongPath :: Graph a -> GraphPath -> [Graph a]
nodesAlongPath n [] = [n]
nodesAlongPath Empty _ = [Empty]
nodesAlongPath (Blank l r) (GraphLeft : xs) = Blank l r : nodesAlongPath l xs
nodesAlongPath (Blank l r) (GraphRight : xs) = Blank l r : nodesAlongPath r xs
nodesAlongPath (Node el l r) (GraphLeft : xs) = Node el l r : nodesAlongPath l xs
nodesAlongPath (Node el l r) (GraphRight : xs) = Node el l r : nodesAlongPath r xs

nodesToEls :: [Graph a] -> [a]
nodesToEls [] = []
nodesToEls (Node el _ _ : xs) = el : nodesToEls xs
nodesToEls (x : xs) = nodesToEls xs

elsAlongPath :: Graph a -> GraphPath -> [a]
elsAlongPath g p = nodesToEls $ nodesAlongPath g p

followNode :: Graph a -> GraphPath -> Maybe (Graph a)
followNode g p = tryGetLast $ nodesAlongPath g p
    where
        tryGetLast [] = Nothing
        tryGetLast lst | length lst < length p = Nothing
        tryGetLast lst = Just (last lst)

follow :: Graph a -> GraphPath -> Maybe a
follow graph path = case followNode graph path of
    Just (Node a _ _) -> Just a
    _ -> Nothing