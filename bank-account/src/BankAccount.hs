module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where
import Control.Concurrent

type BankAccount = MVar Integer

closeAccount :: BankAccount -> IO ()
closeAccount = const $ return ()

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = tryTakeMVar

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount =
--  undefined
  modifyMVar_ account (inc amount)
  >>= getBalance

inc :: Integer -> Integer -> IO Integer
inc a= return . (+ a) >>= readMVar
--  return (getBalance a)
--putMVar ((+) amount) <$> account

openAccount :: IO BankAccount
openAccount = newMVar 0
