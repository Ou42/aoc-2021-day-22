{-# LANGUAGE OverloadedStrings #-}

module SolutionChoices where

import Test.Hspec

import Cuboid (volume)
import RebootStep (generateRemnant, parseInputText)
import Remnant (Remnant)
import qualified RunningCode as RC

inputFilePath :: String
inputFilePath = "data/Day-22-INPUT.txt"

-- data InputData = InputData
--   { inputFilePath :: String
--   , inputLines :: [String]
--   }

-- data ExpectedResult = ExpectedResult
--   { author :: String
--   , description :: String
--   , solver :: String -> Int
--   , correctAnswer :: Int
--   , inputLineCount :: Int
--   }

-- expectedResults :: [ExpectedResult]
-- expectedResults =
--   [ ExpectedResult { description = "Part A"
--                    , solver
--   ]

-- implementations :: [Implementation]
-- implementations =
--   [ Implementation  { name = "Jason"
--                     , inputFilePath = "data/Day-22-INPUT-test.txt"
--                     , solver = RC.solvePuzzle
--                     , correctAnswer = 39769202357779
--                     , inputLines = []
--                     }
--   , Implementation  { name = "Scott"
--                     , inputFilePath = "data/Day-22-INPUT-test.txt"
--                     , solver = scottSolver
--                     , correctAnswer = 39769202357779
--                     , inputLines = []
--                     }
  -- , Implementation  { name = "Scott"
  --                   , inputFilePath = "data/Day-22-INPUT-test.txt"
  --                   , solver = scottSolver
  --                   , correctAnswer = 39769202357779
  --                   , inputLines = []
  --                   }
  -- , Implementation  { name = "Jason"
  --                   , inputFilePath = "data/i3.txt"
  --                   , solver = RC.solvePuzzle
  --                   , correctAnswer = 547647
  --                   , inputLines = []
  --                   }
  -- , Implementation  { name = "Jason"
  --                   , inputFilePath = "data/i4.txt"
  --                   , solver = RC.solvePuzzle
  --                   , correctAnswer = 321769 -- 231540 (2 rb)
  --                   }
  -- , Implementation  { name = "Scott"
  --                   , inputFilePath = "data/i4.txt"
  --                   , solver = scottSolver
  --                   , correctAnswer = 321769 -- 231540 (2 rb)
  --                   , inputLines = []
  --                   }
  -- , Implementation  { name = "Scott"
  --                   , inputFilePath = "data/i3.txt"
  --                   , solver = scottSolver
  --                   , correctAnswer = 547647
  --                   , inputLines = []
  --                   }
  -- ]

filePath :: String
filePath =
  inputFilePath

solveIt :: String -> IO ()
solveIt inputText =
  hspec (specWith inputText)

scottSolver :: [Char] -> (Int, Int)
scottSolver inputText =
  let
    remnant = generateRemnant $ parseInputText inputText
    partA = reduceToTotalVolume $ calculatePartA remnant
    partB = reduceToTotalVolume remnant
  in
    (partA, partB)

reduceToTotalVolume :: Remnant -> Int
reduceToTotalVolume = sum . map volume

calculatePartA :: Remnant -> Remnant
calculatePartA = id

specWith :: String -> Spec
specWith inputText = do
  describe "Scott's should match Jason's Test Sequence" $ do
    it "should run Part B" $ do
      scottSolver inputText
        `shouldBe`
          (RC.solveAv2 inputText, RC.solvePuzzle inputText)
  -- describe "Cuboid" $ do cuboidSpec
  -- describe "SegmentTest" $ do segmentTestSpec
  -- describe "RebootStep" $ do rebootStepSpec
  -- describe "Remnant" $ do remnantSpec

