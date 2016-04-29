-- Asks for your name until you enter it
-- Then greets you.

getName :: IO String
getName = do name <- getLine
             if (length name) < 1
             then do putStrLn "What is your name?"
                     getName
             else return name

main :: IO ()
main = do putStrLn "What is your name?"
          name <- getName
          putStrLn $ "Hi, " ++ name ++ "!"

        
        