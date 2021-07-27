{-# LANGUAGE RankNTypes #-}
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

import           Prelude hiding (concat, filter, foldr, length, map, reverse,
                          (++))

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f z (x:xs) = let z' = f z x in
  z' `seq` foldl' f z' xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys

length :: [a] -> Int
length = foldl' (\l _ -> l + 1) 0

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

filter :: forall a. (a -> Bool) -> [a] -> [a]
filter p = foldr (\y acc -> if p y then y:acc else acc) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []
