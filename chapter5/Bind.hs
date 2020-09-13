module Bind where

printLonger :: IO ()
printLonger = putStr "Enter the first string: " >>= \_ ->
                getLine >>= \first ->
                  putStr "Enter the second string: " >>= \_ ->
                    getLine >>= \second ->
                      let firstlen = length first
                          secondlen = length second in
                          putStrLn (show (max firstlen secondlen))