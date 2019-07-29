import Data.Char
import Data.List
import System.IO
import System.Random

{-
This is the hangman game that runs on haskell.
The program will get the word from the words.txt.
The user will get 9 tries to guess a charecter of 
the word.

-}

-- this is the filepath from where it will get the word
filePath :: IO [String]
filePath = do
    contents <- readFile "words.txt"
    return $ words contents

-- this is to get a random word out of all 
findWord :: [String] -> IO String
findWord xs = do
    i <- randomRIO (0, length xs-1)
    return (xs !! i)

-- this is the word that is to be guessed
answer :: [Char] -> String -> String
answer cs = map (\ c -> if c `elem` cs then c else '_' )

-- this just provides the answer here
showWord :: [Char] -> String -> IO ()
showWord guessed target = putStr $ answer guessed target

-- next guess by the user 
nGuess :: [Char] -> IO [Char]
nGuess cs = do
    putStrLn "Guess charecter"
    c <- getChar
    putStrLn ""
    let c' = [toLower c]    
    return $ c' `union` cs

-- shows how many chances the user has left now 
printChances :: Int -> IO ()
printChances n = putStrLn $ replicate n '|'

-- checks everytime if the user got the correct answer yet
isItCorrect :: [Char] -> String -> Bool
isItCorrect cs target = answer cs target == target

-- loops the game until the user runs out of all the chances
gLoop :: [Char] -> String -> Int -> IO ()
gLoop cs target n
    | isItCorrect cs target = putStrLn "Nice work"
    | n == 0 = putStrLn $ "Game ended. The actual word was" ++ target ++ "."          
    | otherwise    = do
        putStrLn "Keep Guessing"
        printChances n
        
        let oldState = answer cs target
        putStrLn $ oldState ++ "\n"
        
        cs'      <- nGuess cs
        let newState = answer cs' target
        
        case cs' == cs of
            True  -> do putStrLn "Incorrect choice, keep guessing!"   -- illegal input
                        gLoop cs target n
            False -> case oldState == newState of
                True  -> do putStrLn "You lost!" -- unsuccessful guess 
                            gLoop cs' target (n - 1)
                False -> do putStrLn "Good job. Keep Guessing!"
                            gLoop cs' target n

-- main method that will just start the game
main :: IO ()
main = do
    putStrLn "Guess the word."
    dict <- filePath
    target <- findWord dict
    gLoop [] target 9