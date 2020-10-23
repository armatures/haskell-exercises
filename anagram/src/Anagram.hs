module Anagram (anagramsFor) where
import Data.List
import Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss =
  let
    toComparable = (sort . downcase )
    downcase = fmap toLower
  in
    filter (\candidate ->
      (toComparable xs) == (toComparable candidate) &&
        (downcase xs /= downcase candidate)
      ) xss
