module Day22 where

import SolutionChoices (inputPathJason, inputPathScott, solveIt)

solvePuzzle :: IO ()
solvePuzzle = do
  inputTextJason <- readFile inputPathJason
  inputTextScott <- readFile inputPathScott
  solveIt (inputTextJason, inputTextScott)

