-- This phonebook implementation is an example of using MVars as locks.
-- Many threads can call `addContact` at the same time, 
--   but because the method checks out phonebook at the beginning of execution,
--   it is invisible to the rest of threads.


import Control.Monad
import Control.Concurrent
import Data.Map as Map

type Name        = String
type PhoneNumber = String
type PhoneBook   = Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

newPhoneBook :: IO PhoneBookState
newPhoneBook = do
    book <- newMVar Map.empty
    return (PhoneBookState book)
    
addContact :: PhoneBookState -> Name -> PhoneNumber -> IO ()
addContact (PhoneBookState book) name number = do
    currentBookState <- takeMVar book
    putMVar book (Map.insert name number currentBookState)
    

lookupByName :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookupByName (PhoneBookState book) name = do
     currentBookState <- takeMVar book
     putMVar book currentBookState
     return (Map.lookup name currentBookState)


main = do
    phoneBook <- newPhoneBook
    addContact phoneBook "name1" "123-123-1234"
    result <- lookupByName phoneBook "name1"
    putStrLn (show result)
    
