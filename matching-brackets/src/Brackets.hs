module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired s = (==) (Just []) (tryParse Nothing s )

{-| tryParse returns Just the remainder of the string if it succeeds, or Nothing if it fails -}
tryParse :: Maybe Char -> String -> Maybe String
tryParse Nothing [] = Just []
tryParse (Just _) [] = Nothing
tryParse soughtBracket (x:xs)
      | not (elem x "[]{}()") = tryParse soughtBracket xs
      | soughtBracket == (Just x) = Just xs
      | otherwise =
        (case x of
          '{' ->  tryParse (Just '}') xs
          '[' ->  tryParse (Just ']') xs
          '(' ->  tryParse (Just ')') xs
          _ ->
              Nothing
        )
          >>= tryParse soughtBracket
