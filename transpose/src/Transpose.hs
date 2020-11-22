module Transpose (transpose, padFirstRowsMost) where

import Data.List (tails)


transpose :: [String] -> [String]
transpose [] = []
transpose rows = foldl (\ acc row-> attachColumn (oneRowToColumn row) acc ) initAcc padded
  where
    padded = padFirstRowsMost rows
    newColLength= maximum (fmap length rows)
    initAcc = replicate newColLength []

oneRowToColumn :: String -> [String]
oneRowToColumn =
  foldl (\acc x -> acc ++ [[x]]) []

attachColumn :: [String] -> [String] -> [String]
attachColumn newCol grid = zipWith (++) grid newColPadded
  where newColPadded = padRight [] newCol (length grid)

padFirstRowsMost :: [String] -> [String]
padFirstRowsMost []=[]
padFirstRowsMost rows=
  zipWith (padRight ' ') rows padLengths
  where
  padLengths = maximum . fmap length <$> init (tails rows)

padRight :: a -> [a] -> Int -> [a]
padRight pad word desiredLength =
  let spaceLength = desiredLength - length word
  in
  word ++ replicate spaceLength pad
