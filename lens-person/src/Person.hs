{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  ) where

import Data.Time.Calendar (fromGregorian, toGregorian, Day)
import Control.Lens
import Control.Applicative

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     }

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 }

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 }

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       }

makeLenses ''Address
makeLenses ''Born
makeLenses ''Name
makeLenses ''Person

bornStreet :: Born -> String
bornStreet b = b ^. bornAt ^. street

setCurrentStreet :: String -> Person -> Person
setCurrentStreet s = (address . street) .~ s

setBirthMonth :: Int -> Person -> Person
setBirthMonth = 
  set (born . bornOn . gregorian . month)

month :: Lens' (a,Int,c) Int
month = _2

gregorian :: Lens' Day (Integer, Int, Int)
gregorian = lens (toGregorian) (const (uncurry2 fromGregorian) )

uncurry2 :: forall t1 t2 t3 t4. (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
uncurry2 f (x,y,z)= f x y z

renameStreets :: (String -> String) -> Person -> Person
renameStreets = 
  -- reference p1202 in "Monads Gone Wild" in the Haskell Book
  -- (over l1 f) . (over l2 f)
  liftA2 (.) (over l1) (over l2) 
  where
    l1 :: Lens' Person String
    l1 = address . street

    l2 :: Lens' Person String
    l2 = born . bornAt . street
