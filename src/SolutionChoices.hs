{-# LANGUAGE OverloadedStrings #-}

module SolutionChoices where

import Test.Hspec

import Cuboid (volume, Source(..), Target(..))
import RebootStep (generateRemnant, parseInputText)
import Remnant (Remnant, emptyRemnant, mkAxisResults)
import qualified RunningCode as RC
import Segment (AdjLeft(..), AdjRight(..), AxisResult(..), Overlap(..), SrcSeg(..), TrgSeg(..))

inputPathJason :: String
inputPathJason = "data/Day-22-INPUT.txt"

inputPathScott :: String
inputPathScott = "data/Day-22-input-ofd.txt"

solveIt :: (String, String) -> IO ()
solveIt twoInputFiles =
  hspec (specWith twoInputFiles)

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

specWith :: (String, String) -> Spec
specWith (inputTextJason, inputTextScott) = do
  describe "Puzzle Answer for Jason's file" $ do
    let (partA, partB) = scottSolver inputTextJason
    context "Exercise" $ do
      it "Part A cube count" $ do
        partA
          `shouldBe`
            577205
      it "Part B cube count" $ do
        partB
          `shouldBe`
            1197308251666843
  describe "Puzzle Answer for Scott's file" $ do
    let (partA, partB) = scottSolver inputTextScott
    context "Exercise" $ do
      it "Part A cube count" $ do
        partA
          `shouldBe`
            598616
      it "Part B cube count" $ do
        partB
          `shouldBe`
            1193043154475246

