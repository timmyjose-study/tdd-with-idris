module DataStore

import Data.List
import Data.Strings
import Data.Vect
import System.REPL

%default total

infixr 5 .+.

data Schema = SChar 
            | SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SChar = Char
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

parsePrefix : (schema : Schema) -> (inp : String) -> Maybe (SchemaType schema, String)
parsePrefix SChar inp = getSingleQuoted (unpack inp) 
  where
    getSingleQuoted : List Char -> Maybe (Char, String)
    getSingleQuoted ('\'' :: xs) = case xs of 
                                        (c :: '\'' :: rest) => Just (c, ltrim (pack rest))
                                        _ => Nothing
    getSingleQuoted _ = Nothing
parsePrefix SString inp = getQuoted (unpack inp)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
                                 (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                 _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt inp = case span isDigit inp of
                            ("", rest) => Nothing
                            (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schemal .+. schemar) inp = 
  do (lval, inp') <- parsePrefix schemal inp 
     (rval, inp'') <- parsePrefix schemar inp'
     Just ((lval, rval), inp'')

stringToSchema : (schema : Schema) -> (inp : String) -> Maybe (SchemaType schema)
stringToSchema schema inp = 
  case parsePrefix schema inp of
       Just (res, "") => Just res
       Just _ => Nothing
       Nothing => Nothing

schemaToString : { schema : Schema } -> SchemaType schema -> String
schemaToString {schema = SChar} item = show item
schemaToString {schema = SString} item = show item
schemaToString {schema = SInt} item = show item
schemaToString {schema = (x .+. y)} (itemx, itemy) = 
  schemaToString itemx ++ ", " ++ schemaToString itemy

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> (item : SchemaType (schema store)) -> DataStore
addToStore (MkData schema size items) newItem = MkData _ _ (addItem items)
  where
    addItem : Vect oldsize (SchemaType schema) -> Vect (S oldsize) (SchemaType schema)
    addItem [] = [newItem]
    addItem (item :: items) = item :: addItem items

getEntry : (store : DataStore) -> (idx : Integer) -> String
getEntry (MkData _ size items) idx = case integerToFin idx size of
                                      Nothing => "invalid index: " ++ show idx
                                      Just idx' => schemaToString (index idx' items)

getAllEntries : (store : DataStore) -> String
getAllEntries store = getAllEntriesHelper (size store) ""
  where
    getAllEntriesHelper : (idx : Nat) -> (results : String) -> String
    getAllEntriesHelper Z results = results
    getAllEntriesHelper (S idx') results = getAllEntriesHelper idx' results ++ "\n" ++ getEntry store (cast idx')

data Command : Schema -> Type where
  SetSchema : (newschema : Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Maybe Integer -> Command schema
  Size : Command schema
  Quit : Command schema

parseSchema : List String -> Maybe Schema
parseSchema ("Char" :: xs) = 
  case xs of
       [] => Just SChar
       _ => case parseSchema xs of
                 Nothing => Nothing
                 Just schema' => Just (SChar .+. schema')
parseSchema ("String" :: xs) = 
  case xs of
       [] => Just SString
       _ => case parseSchema xs of
                 Nothing => Nothing
                 Just schema' => Just (SString .+. schema')
parseSchema ("Int" :: xs) =
  case xs of
       [] => Just SInt
       _ => case parseSchema xs of
                 Nothing => Nothing
                 Just schema' => Just (SInt .+. schema')
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" item = case stringToSchema schema item of
                                      Nothing => Nothing
                                      Just item' => Just (Add item')
parseCommand schema "get" idxStr = 
  if idxStr == "" 
     then Just (Get Nothing)
     else case all isDigit (unpack idxStr) of
             False => Nothing
             True => Just $ Get (Just (cast idxStr))
parseCommand schema "size" "" = Just Size
parseCommand schema "setschema" newschema = 
  case parseSchema (words newschema) of
       Nothing => Nothing
       Just schema => Just (SetSchema schema)
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (inp : String) -> Maybe (Command schema)
parse schema inp = let (cmd, args) = span (/= ' ') inp in
                       parseCommand schema cmd (ltrim args)

setSchema : (store : DataStore) -> (newschema : Schema) -> Maybe DataStore
setSchema store newschema = case size store of
                                 Z => Just (MkData newschema 0 [])
                                 _ => Nothing

processCommand : (store : DataStore) -> (cmd : Command (schema store)) -> Maybe (String, DataStore)
processCommand store (Add item) = Just ("ID" ++ show (size store) ++ "\n", addToStore store item)
processCommand store (Get idx) = 
  case idx of
       Nothing => Just (getAllEntries store ++ "\n", store)
       Just idx' => Just (getEntry store idx' ++ "\n", store)
processCommand store Size = Just (show (size store) ++ "\n", store)
processCommand store (SetSchema schema) = 
  case setSchema store schema of
       Nothing => Just ("Cannot update schema - store is possibly not empty\n", store)
       Just store' => Just ("updated schema\n", store')
processCommand store Quit = Nothing

processInput : (store : DataStore) -> (inp : String) -> Maybe (String, DataStore)
processInput store inp = 
  case parse (schema store) inp of
       Nothing => Just ("invalid command\n", store)
       Just cmd => processCommand store cmd

partial
main : HasIO io => io ()
main = replWith (MkData (SString .+. SString .+. SInt) 0 []) "Command: " processInput