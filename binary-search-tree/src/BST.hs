module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Leaf | Node a (BST a) (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft tree =
    case tree of
        Leaf -> Nothing
        Node x left _ -> Just left

bstRight :: BST a -> Maybe (BST a)
bstRight tree =     case tree of
        Leaf -> Nothing
        Node x _ right -> Just right

bstValue :: BST a -> Maybe a
bstValue tree = case tree of
    Leaf -> Nothing
    Node a _ _ -> Just a

empty :: BST a
empty = Leaf

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert x tree = case tree of
    Leaf -> Node x Leaf Leaf
    Node y left right -> if x > y then 
        Node y left (insert x right) 
        else
        Node y (insert x left) right

singleton :: a -> BST a
singleton x = Node x Leaf Leaf

toList :: BST a -> [a]
toList tree =
    case tree of
        Leaf -> []
        Node x left right -> toList left ++ [x] ++ toList right
