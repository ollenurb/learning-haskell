module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w = let l = length (w :: String)
                         in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord list = do
    randomIndex <- randomRIO (0, length list-1)
    return $ list !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $
         fmap renderPuzzleChar discovered)
         ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle str =
    Puzzle str (map (const Nothing) str) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _ ) c = elem c str

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ str) c = elem c str

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c)  = c
renderPuzzleChar (Nothing) = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c
    = Puzzle word newFilledInSoFar (c : s)
        where
            zipper guessed wordChar guessChar =
                if wordChar == guessed
                then Just wordChar
                else guessChar
            newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do putStrLn "You already guess that character, pick something else!"
                        return puzzle
        (True, _) -> do putStrLn "This character was in the word, filling in.."
                        return (fillInCharacter puzzle guess)
        (False,_) -> do putStrLn "This character wasn't in the word, try again"
                        return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed)
    | length guessed > 7 = do putStrLn "You Lost!"
                              putStrLn $ "The word was: " ++ wordToGuess
                              exitSuccess
    | otherwise = return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _)
    | all isJust filledInSoFar = putStrLn "You Won!" >> exitSuccess
    | otherwise = return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _ -> putStrLn "Your guess myst be a single character!"

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
