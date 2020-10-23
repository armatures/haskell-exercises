module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite (x:xs) = recite' (x:xs) ++ "And all for the want of a " ++ x ++ "."

recite' :: [String] -> String
recite' (x:x':xs) = "For want of a " ++ x ++ " the " ++ x' ++" was lost.\n" ++ (recite' (x':xs))
recite' _ = ""

