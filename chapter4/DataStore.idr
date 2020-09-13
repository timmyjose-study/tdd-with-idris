module DataStore

import Data.Strings
import Data.Vect
import System.REPL

%default total

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' _) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

addToStore : (store : DataStore) -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (item :: items) = item :: addToData items

data Command = Add String
             | Get Integer
             | Quit 

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (store : DataStore) -> (pos : Integer) -> Maybe (String, DataStore)
getEntry store pos = case integerToFin pos (size store) of
                          Nothing => Just ("Invalid index: " ++ show pos ++ "\n", store)
                          Just idx => Just (show (index idx (items store)) ++ "\n", store)

processCommand : (store : DataStore) -> (cmd : Command) -> Maybe (String, DataStore)
processCommand store (Add item) = Just ("ID" ++ show (size store) ++ "\n", addToStore store item)
processCommand store (Get idx) = getEntry store idx
processCommand store Quit = Nothing

processInput : (store : DataStore) -> String -> Maybe (String, DataStore)
processInput store inp =
  case parse inp of
       Nothing => Just ("Invalid command\n", store)
       Just cmd => processCommand store cmd

partial
main : IO ()
main = replWith (MkData _ []) "Command: " processInput