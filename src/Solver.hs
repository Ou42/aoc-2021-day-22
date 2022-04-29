module Solver where

import CalculatePartA (calculatePartA)
import Cuboid (volume)
import RebootStep (generateRemnant, parseInputText)

{- | The (almost complete) topdown view of the solution
-}
solver :: [Char] -> (Int, Int)
solver inputText =
  let
    remnant =                                     -- Remnant is all the non-intersecting target cuboids;
      generateRemnant $ parseInputText inputText  -- requires the most calculating to obtain.
    partAanswer = sum $ map volume $ calculatePartA remnant -- Adjust remnant for Part A answer
    partBanswer = sum $ map volume                  remnant -- Use    remnant for Part B answer
  in
    (partAanswer, partBanswer)                    -- Provide both answers
