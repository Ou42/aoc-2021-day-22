module Day22 where

import SolutionChoices (filePath, solutionToUse, solveIt)

solvePuzzle :: IO ()
solvePuzzle = do
  inputText <- readFile filePath
  print $ solveIt inputText

