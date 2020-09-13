module Main

main : IO ()
main = do putStr "Enter your name: "
          name <- getLine
          putStrLn $ "Nice to meet you, " ++ name ++ "!"