module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f z (x:xs) = let z' = f z x in
  z' `seq` foldl' f z' xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z xs = foldr f (f (last xs) z) (init xs)

length :: [a] -> Int
length xs = foldl' (\l _ -> l + 1) 0 xs

reverse :: [a] -> [a]
reverse xs = foldl (flip (:)) [] xs

map :: (a -> b) -> [a] -> [b]
map f xs = foldr ((:) . f) [] xs

filter :: (a -> Bool) -> [a] -> [a]
filter p xs =
  let
    -- h :: a -> [a] -> [a]
    h y ys = if p y then
              y:ys
            else
              ys
  in
    foldr h [] xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
xs ++ ys = (init xs) ++ ((last xs) : ys)

concat :: [[a]] -> [a]
concat = foldr (++) []
