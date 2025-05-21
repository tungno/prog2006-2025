-- Export task2Main function from Task2 module
module Task2
   ( task2Main
   ) where

-- Type signature: task2Main is an IO action
task2Main :: IO ()
-- 'do' block for sequence of IO actions
task2Main = do
   putStrLn "What is your name?"     -- Print prompt
   name <- getLine                    -- Read user input into 'name'
   putStrLn $ "Hello " ++ name       -- Print greeting with user's name