module Ex5

printLonger : HasIO io => io ()
printLonger = do putStr "Enter the first string: "
                 first <- getLine
                 putStr "Enter the second string: "
                 second <- getLine
                 let firstlen = length first -- no `in` here
                 let secondlen = length second
                 putStrLn $ show (max firstlen secondlen)

printLongerBind : HasIO io => io ()
printLongerBind = putStr "Enter the first string: " >>= \_ =>
                    getLine >>= \first =>
                      putStr "Enter the second string: " >>= \_ =>
                        getLine >>= \second =>
                          let firstlen = length first 
                              secondlen = length second in -- `in` is used here - why the inconsistency?
                            putStrLn (show (max firstlen secondlen))