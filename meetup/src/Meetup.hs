{-# LANGUAGE LambdaCase #-}

module Meetup (Weekday(..), Schedule(..), meetupDay) where

import qualified Data.Time.Calendar as C

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
toDayOfWeek :: Weekday -> C.DayOfWeek
toDayOfWeek = \case
              Monday -> C.Monday
              Tuesday -> C.Tuesday
              Wednesday -> C.Wednesday
              Thursday -> C.Thursday
              Friday -> C.Friday
              Saturday -> C.Saturday
              Sunday -> C.Sunday

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> C.Day
meetupDay schedule weekday year month =
  let
    monthDays = C.fromGregorian year month <$> [1 .. (C.gregorianMonthLength year month)]
  in
    case filter (\d -> C.dayOfWeek d == toDayOfWeek weekday) monthDays of
      (a:b:c:d:e) ->
        case schedule of
          First -> a
          Second -> b
          Third -> c
          Fourth -> d
          Last -> case e of
            []  -> d
            x:_ -> x
          Teenth -> if (dayOfMonth . C.toGregorian) b < 13 then c else b
      _ ->
        head monthDays

dayOfMonth (_y,_m,d)= d
