module IsbnVerifier (isbn) where
import Data.Char (digitToInt, isDigit)
import Control.Applicative (liftA2)

isbn :: String -> Bool
isbn s = isValid (toInt (removeDashes s))

isValid :: Maybe [ Int ] -> Bool
isValid Nothing = False
isValid (Just xs) =
  if length xs /= 10 then
    False
  else
  (==) 0 $
    flip mod 11 (
      sum $ zipWith (*) xs [10,9..1 ]
      )

removeDashes :: String -> String
removeDashes = filter (\x -> isDigit x || x == 'X' )

toInt :: String -> Maybe [Int]
toInt ('X':[]) =
     Just [10]

toInt (x:xs) =
  if isDigit x then
    liftA2 (:) (Just $ digitToInt x) (toInt xs)
  else
    Nothing

toInt []=Just []