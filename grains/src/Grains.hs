module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
  | n > 0 && n < 65 = Just (2 ^ (n - 1))
  | otherwise = Nothing

total :: Integer
total = maybe 0 sum . mapM square $ [1 .. 64]
