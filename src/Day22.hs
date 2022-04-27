module Day22 where

import SolutionChoices (inputFilePath, solutionToUse, solveIt)

solvePuzzle :: IO ()
solvePuzzle = do
  inputText <- readFile inputFilePath
  print $ solveIt inputText

