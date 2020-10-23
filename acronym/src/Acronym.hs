module Acronym (abbreviate) where
import Data.Char

abbreviate :: String -> String
abbreviate =
  let
    splitCamelCase :: String -> String
    splitCamelCase l=
      if (any isUpper l) && (any isLower l) then
        filter isUpper l
      else
        [head l]
    wordChars :: Char -> Bool
    wordChars c = isAlpha c || elem c "'"
    words' s=  case dropWhile (not . wordChars) s of
                 "" -> []
                 s' -> w : words' s''
                     where (w, s'') =
                              break (not . wordChars) s'
  in
    (fmap toUpper) . concat . (fmap splitCamelCase) . words'
