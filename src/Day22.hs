module Day22 where

import SolutionChoices (inputPathJason, inputPathScott, solveIt)

{- | At the beginning of the program, acquired all possible
   | input file contents so that the remainder of the program
   | can be pure.
-}
solvePuzzle :: IO ()
solvePuzzle = do
  inputTextJason <- readFile inputPathJason
  inputTextScott <- readFile inputPathScott
  solveIt (inputTextJason, inputTextScott)
