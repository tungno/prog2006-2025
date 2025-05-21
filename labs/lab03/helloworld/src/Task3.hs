-- Version A: Type-Safe Implementation
module Task3 
    ( task3Main
    , Age        -- Export type but not constructor
    , makeAge    -- Export smart constructor
    , addAge
    , addNumber
    ) where

-- Define Age type with a private constructor
-- Using Int as the backing type
newtype Age = MkAge Int deriving Show

-- Smart constructor ensures type safety
makeAge :: Int -> Maybe Age
makeAge n | n >= 0    = Just (MkAge n)
         | otherwise = Nothing

-- Generic function that works on any numerical type
-- should only take Age instances as parameters value, and return a new Age instance that is a sum of the two Age instances
-- Num typeclass provides basic operations like +, -, * and so on..
addNumber :: Num a => a -> a -> a
addNumber x y = x + y

-- Age-specific addition that only works with Age type
-- Should only take Age instances as parameters and return a new Age instance, that is a sum of the two Age instances
addAge :: Age -> Age -> Age
addAge (MkAge x) (MkAge y) = MkAge (x + y)

-- Safe age parsing from string
readAge :: String -> Maybe Age
readAge s = case reads s of
    [(n, "")] -> makeAge n
    _         -> Nothing

-- Main program
task3Main :: IO ()
task3Main = do
    putStrLn "Hi, what is your name?"
    name <- getLine
    putStrLn "and what is your age?"
    ageStr <- getLine
    case readAge ageStr of
        Just age -> 
            case addAge age (MkAge 10) of
                MkAge finalAge -> 
                    putStrLn $ "Hello " ++ name ++ ", in 10 years you will be " ++ 
                              show finalAge ++ "."
        Nothing -> 
            putStrLn "Please enter a valid age (positive number)."

{-
-- Version B: Less Restrictive Implementation
module Task3 
    ( task3Main
    , Age(..)    -- Export constructor
    , addAge
    , addNumber
    ) where

-- Define Age type with public constructor
newtype Age = Age Double deriving Show

-- Generic number addition with Num constraint
addNumber :: Num a => a -> a -> a
addNumber x y = x + y

-- Age-specific addition but constructor is public
addAge :: Age -> Age -> Age
addAge (Age x) (Age y) = Age (x + y)

-- Safe age parsing from string
readAge :: String -> Maybe Age
readAge s = case reads s of
    [(n, "")] -> if n >= 0 then Just (Age n) else Nothing
    _         -> Nothing

-- Main program
task3Main :: IO ()
task3Main = do
    putStrLn "Hi, what is your name?"
    name <- getLine
    putStrLn "and what is your age?"
    ageStr <- getLine
    case readAge ageStr of
        Just age -> 
            let finalAge = addAge age (Age 10)
            in putStrLn $ "Hello " ++ name ++ ", in 10 years you will be " ++ 
                         show finalAge ++ "."
        Nothing -> 
            putStrLn "Please enter a valid age (positive number)."

-}
{- 
Key Differences Between Version A and B:

1. Type Safety:
   Version A (More Type-Safe):
   - Constructor (MkAge) is not exported
   - Uses smart constructor makeAge for validation
   - Prevents direct creation of Age values
   - Forces use of makeAge which includes validation
   Use when: You need to ensure data invariants and prevent invalid states

   Version B (Less Restrictive):
   - Constructor (Age) is exported via Age(..)
   - Allows direct creation of Age values
   - No enforced validation at compile time
   Use when: Performance is critical or when working with trusted data

2. Usage Context:
   Version A:
   - Better for public APIs where data validation is crucial
   - Ensures all Age values are valid throughout the program
   - Slightly more verbose due to Maybe handling

   Version B:
   - Suitable for internal modules where data is trusted
   - More flexible but less safe
   - Simpler to use but might allow invalid states

3. Implementation Choice:
   Choose Version A when:
   - Working on public-facing libraries
   - Data validation is crucial
   - You need to maintain invariants
   
   Choose Version B when:
   - Working with internal, trusted code
   - Performance is critical
   - The extra safety is not worth the overhead
-}