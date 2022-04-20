{-# LANGUAGE OverloadedStrings #-}

module SolutionChoices where

import Cuboid (volume)
import RebootStep (generateRemnant, parseInputText)
import Remnant (Remnant)
import qualified RunningCode as RC

data Implementation = Implementation
  { name :: String
  , inputFilePath :: String
  , solver :: String -> Int
  }

jasonImplementation :: Implementation
jasonImplementation =
  Implementation { name = "Jason"
                 , inputFilePath = "data/Day-22-INPUT.txt"
                 , solver = RC.solvePuzzle
                 }

scottImplementation :: Implementation
scottImplementation =
    Implementation { name = "Scott"
                   , inputFilePath = "data/Day-22-INPUT.txt"
                   , solver = scottSolver
                   }

{- This specifies what solution to use!!!
-}
solutionToUse :: Implementation
solutionToUse = jasonImplementation

filePath :: String
filePath =
  inputFilePath solutionToUse

solveIt :: [Char] -> [Char]
solveIt inputText =
  solveSpecifically inputText jasonImplementation -- which solution is selected

solveSpecifically :: [Char] -> Implementation -> [Char]
solveSpecifically inputText impl = do
  returnAnswer impl ((solver impl) inputText)

returnAnswer :: Implementation -> Int -> [Char]
returnAnswer i solverResult =
  name i <> "'s ANSWER: " <> show solverResult

scottSolver :: [Char] -> Int
scottSolver inputText =
  sum $ map volume $ generateRemnant $ parseInputText inputText

