module Pangram (isPangram) where
import Data.List
import Data.Maybe
import Data.Char

isPangram :: String -> Bool
isPangram =
  let
    alphabet = ['a'..'z']
  in
  (==) alphabet .
  sort .
  nub .
  filter (flip elem alphabet) .
  map toLower
