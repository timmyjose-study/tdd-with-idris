module Ex4_1

import Data.Strings
import Data.Vect
import System.REPL

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : (store : DataStore) -> Nat
size (MkData size' _) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

addToStore : (store : DataStore) -> (item : String) -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : (items : Vect old String) -> Vect (S old) String
    addToData [] = [newItem]
    addToData (item :: items) = item :: addToData items

searchStore : (store : DataStore) -> (word : String) -> String
searchStore (MkData _ items) word = searchHelper items 0 word ""
  where
    searchHelper : Vect n String -> (idx : Nat) -> (word : String) -> (results : String) -> String
    searchHelper [] idx word results = results
    searchHelper (item :: items) idx word results = 
      case isInfixOf word item of
           True => searchHelper items (idx + 1) word (results ++ show (idx + 1) ++ " " ++ item ++ "\n")
           False => searchHelper items (idx + 1)  word results

empty : DataStore
empty = MkData 0 []

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

processCommand : (store : DataStore) -> (cmd : Command) -> Maybe (String, DataStore)
processCommand store (Add str) = Just ("ID" ++ show (size store) ++ "\n", addToStore store str)
processCommand store (Get idx) = case integerToFin idx (size store) of
                                Nothing => Just ("Invalid index: " ++ show idx ++ "\n", store)
                                Just pos => Just (index pos (items store) ++ "\n", store)
processCommand store (Search str) = Just (searchStore store str ++ "\n", store)                                
processCommand store Size = Just ("Number of items: " ++ show (size store) ++ "\n", store)
processCommand store Quit = Nothing

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" idxStr = case all isDigit (unpack idxStr) of
                              False => Nothing
                              True => Just (Get (cast idxStr))
parseCommand "search" str = Just (Search str)
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _  = Nothing

parse : String -> Maybe Command
parse inp = let (cmd, args) = span (/= ' ') inp in
                parseCommand cmd (ltrim args)

processInput : (store : DataStore) -> String -> Maybe (String, DataStore)
processInput store inp = 
  case parse inp of
       Nothing => Just ("Invalid command\n", store)
       Just cmd => processCommand store cmd
      

main : IO ()
main = replWith empty "Command: " processInput