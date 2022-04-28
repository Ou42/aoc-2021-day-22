module Day22 where

import SolutionChoices (inputFilePath, solveIt)

solvePuzzle :: IO ()
solvePuzzle = do
  inputText <- readFile inputFilePath
  solveIt inputText

