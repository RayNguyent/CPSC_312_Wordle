import System.Random
import Data.List (intersect, (\\))
import ListofWords
{-# LANGUAGE LambdaCase #-}


wordList :: [String]
wordList = ["which","there","their","about","would","these","other","words","could","write","first"]


generateRandomWord :: [a] -> IO a
generateRandomWord wordList = do
    index <- randomRIO (0, length wordList - 1)
    return $ wordList !! index

-- Define the target word
--targetWord :: String
--targetWord = generateRandomWord

-- Function to check if a character is present in the target word
checkGuess :: Char -> String -> Bool
checkGuess c word = c `elem` word

-- Function to provide feedback on the correctness and position of the guessed word

evaluateGuess :: String -> String -> [State]
evaluateGuess guessWord target =
    let correctChars = intersect guessWord target
        correctPositions = zipWith (\x y -> if x == y then Success else Misplace) guessWord target
        remainingGuesses = guessWord \\ correctChars
        feedback = map (\c -> if c `elem` remainingGuesses then Fail else Success) target
        misplaceIndices = [i | (i, c) <- zip [0..] guessWord, c `elem` target && c /= target !! i]
    in zipWith (\fb idx -> if idx `elem` misplaceIndices then Misplace else fb) feedback [0..]






-- Main game loop
playWordle :: String -> Int -> IO ()
playWordle target attemptsLeft
    | attemptsLeft == 0 = putStrLn $ "Out of attempts. Game over. The answer was: "++ target
    | otherwise = do
        putStrLn $ "Attempts left: " ++ show attemptsLeft
        putStrLn "Enter your guess:"
        guess <- getLine
        let feedback = evaluateGuess guess target
        putStrLn "Feedback:"
        putStrLn (concatMap (\s -> showState s) feedback)

        putStrLn ""
        if guess == target
            then putStrLn "Congratulations! You guessed the word!"
            else playWordle target (attemptsLeft - 1)

data State = Fail | Success | Misplace

showState :: State -> String
showState state = case state of
  Fail -> "\ESC[91mO" -- Red grid
  Success -> "\ESC[32mO" -- Green circle
  Misplace -> "\ESC[33mO" -- Yellow grid





-- Main function
main :: IO ()
main = do
    buffer <- generateRandomWord wordList
    let targetWord = buffer :: String
    putStrLn "Welcome to Wordle!"
    putStrLn "Try to guess the target word in 5 attempts."
    putStrLn "The target word contains 5 letters."
    playWordle targetWord 5
