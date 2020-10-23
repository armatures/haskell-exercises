module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen,choose, vectorOf)
import Data.List (sort)

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier i =
      (i - 10) `div` 2


ability :: Gen Int
ability =
  fmap (sum . (take 3) . sort)
  (vectorOf 4 (choose (1,6)))

character :: Gen Character
character =
    do
      str <- ability
      dex <- ability
      con <- ability
      int <- ability
      wis <- ability
      cha <- ability
      let hitpoints = 10 + (modifier con)
      return $ Character str dex con int wis cha hitpoints