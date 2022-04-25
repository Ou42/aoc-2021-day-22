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
  , correctAnswer :: Int
  }

jasonImplementation :: Implementation
jasonImplementation =
  Implementation { name = "Jason"
                 , inputFilePath = "data/Day-22-INPUT-test.txt"
                 , solver = RC.solvePuzzle
                 , correctAnswer = 39769202357779
                 }

scottImplementation :: Implementation
scottImplementation =
    Implementation { name = "Scott"
                   , inputFilePath = "data/Day-22-INPUT-test.txt"
                   , solver = scottSolver
                   , correctAnswer = 39769202357779
                   }

jasonImplementation_i3 :: Implementation
jasonImplementation_i3 =
  Implementation { name = "Jason"
                 , inputFilePath = "data/i3.txt"
                 , solver = RC.solvePuzzle
                 , correctAnswer = 547647
                 }

scottImplementation_i3 :: Implementation
scottImplementation_i3 =
    Implementation { name = "Scott"
                   , inputFilePath = "data/i3.txt"
                   , solver = scottSolver
                   , correctAnswer = 547647
                   }

{- This specifies what solution to use!!!
-}
solutionToUse :: Implementation
-- solutionToUse = jasonImplementation
-- solutionToUse = scottImplementation
-- solutionToUse = jasonImplementation_i3
solutionToUse = scottImplementation_i3

filePath :: String
filePath =
  inputFilePath solutionToUse

solveIt :: [Char] -> [Char]
solveIt inputText =
  solveSpecifically inputText solutionToUse

solveSpecifically :: [Char] -> Implementation -> [Char]
solveSpecifically inputText impl = do
  returnAnswer impl ((solver impl) inputText)

returnAnswer :: Implementation -> Int -> [Char]
returnAnswer i solverResult =
  name i <> "'s ANSWER: " <> show solverResult <> "; Correct answer is " <> show (correctAnswer solutionToUse)

scottSolver :: [Char] -> Int
scottSolver inputText =
  sum $ map volume $ generateRemnant $ parseInputText inputText

