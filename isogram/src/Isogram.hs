module Isogram
  ( isIsogram
  ) where
import           Data.Char
import           Data.List

isIsogram :: String -> Bool
isIsogram = all (null . tail) . group . sort . map toLower . filter isAlpha
