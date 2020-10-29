module DNA (toRNA) where
import Data.List

toRNA :: String -> Either Char String
toRNA =
  let
  help char =
   case char of
     'A' -> Right 'U'
     'G' -> Right 'C'
     'C' -> Right 'G'
     'T' -> Right 'A'
     otherwise -> Left char
  in
  mapM help


