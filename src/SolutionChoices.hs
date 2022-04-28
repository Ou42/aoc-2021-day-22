{-# LANGUAGE OverloadedStrings #-}

module SolutionChoices where

import Test.Hspec

import Cuboid (volume, Source(..), Target(..))
import RebootStep (generateRemnant, parseInputText)
import Remnant (Remnant, emptyRemnant, mkAxisResults)
import qualified RunningCode as RC
import Segment (AdjLeft(..), AdjRight(..), AxisResult(..), Overlap(..), SrcSeg(..), TrgSeg(..))

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
calculatePartA = foldl truncateTarget emptyRemnant

truncateTarget :: Remnant -> Target -> Remnant
truncateTarget outgoingRemnant target =
  let
    axisResults = mkAxisResults region50Source target
  in
    if NoOverlap `elem` axisResults then
      outgoingRemnant
    else
      let
         (outgoingTarget, _) = foldl sourceTargetIntersection (target, 0) axisResults
      in
      outgoingTarget : outgoingRemnant

{- | generate the overlap cuboid from the compare -}
sourceTargetIntersection :: (Target, Int) -> AxisResult -> (Target, Int)
sourceTargetIntersection (target, axisOffset) axisResult =
   case axisResult of
      Overlaps ->
         (target, axisOffset + 1)
      OverlapsLeft (Overlap overlap) (AdjRight _) ->
         createCommon overlap
      OverlapsRight (AdjLeft _) (Overlap overlap) ->
         createCommon overlap
      OverlappedByTarget (AdjLeft _) (Overlap overlap) (AdjRight _) ->
         createCommon overlap
   where
      createCommon :: TrgSeg -> (Target, Int)
      createCommon overlap' =
         ( createPiece overlap'
         , axisOffset + 1
         )

      {- | Return a torn-off piece of the original cuboid -}
      createPiece :: TrgSeg -> Target
      createPiece segment =
        let
          Target incoming = target
        in
        Target $ take axisOffset incoming <> [segment] <> drop (axisOffset+1) incoming

region50 :: SrcSeg
region50 = SrcSeg (-50, 50)

region50Source = Source [region50, region50, region50]

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

