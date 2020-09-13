module Chapter5

printLength : HasIO io => io ()
printLength = putStr "Enter a string: " >>= \_ =>
                getLine >>= \input =>
                  let len = length input in
                    putStrLn $ "\"" ++ input ++ "\" has length " ++ show len

printTwoThings : HasIO io => io ()
printTwoThings = do putStrLn "Hello"
                    putStrLn "World"

printThreeThings : HasIO io => io ()
printThreeThings = putStrLn "Hello" >>= \_ =>
                    putStrLn "World" >>= \_ =>
                      putStrLn "Again"

printInput : HasIO io => io ()
printInput = do x <- getLine
                putStrLn x

printLengthAgain : HasIO io => io ()
printLengthAgain = do putStr "Enter a string: "
                      string <- getLine
                      let len = length string -- no `in` here
                      putStrLn $ "\"" ++ string ++ "\" has length " ++ show len

readPair : HasIO io => io (String, String)
readPair = do f <- getLine
              s <- getLine
              pure (f, s)

usePair : HasIO io => io ()
usePair = do p <- readPair
             case p of
                  (f, s) => putStrLn $ "You entered the following strings: " ++ f ++ ", " ++ s

-- destructuring pattern-matching

usePairImproved : HasIO io => io ()
usePairImproved = do (f, s) <- readPair
                     putStrLn $ "You entered the following strings: " ++ f ++ ", " ++ s