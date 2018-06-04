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

data LinkedList a = LinkedList a (LinkedList a) | Nil  deriving (Eq, Show)

datum :: LinkedList a -> a
datum (LinkedList dat _) = dat

fromList :: [a] -> LinkedList a
fromList [] = nil
fromList (x: xs) = LinkedList x $ fromList xs

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil l =  False

new :: a -> LinkedList a -> LinkedList a
new x linkedList = LinkedList x linkedList

next :: LinkedList a -> LinkedList a
next (LinkedList _ next) = next

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList linkedList = (fromList .  reverse . toList) linkedList

toList :: LinkedList a -> [a]
toList Nil = []
toList (LinkedList dat next) = dat: toList next
