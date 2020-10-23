module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Item a (LinkedList a)
    | Nil
        deriving (Eq, Show)

datum :: LinkedList a -> a
datum linkedList =
    case linkedList of
        Item x _ ->
            x
        Nil ->
            error "tried to access the datum of a nil list"

fromList :: [a] -> LinkedList a
fromList (x:xs) = Item x (fromList xs)
fromList _ = Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = Item

next :: LinkedList a -> LinkedList a
next (Item _ linkedList) = linkedList
next Nil = Nil

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList (Item x childList) = append x (reverseLinkedList childList)
reverseLinkedList Nil = Nil

append :: a -> LinkedList a -> LinkedList a
append x Nil = Item x Nil
append x (Item y childList) = Item y (append x childList)

toList :: LinkedList a -> [a]
toList Nil = []
toList (Item x xs) = x:(toList xs)
