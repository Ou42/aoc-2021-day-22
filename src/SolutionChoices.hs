module SolutionChoices where

import Test.Hspec (Spec, context, describe, it, hspec, shouldBe)

import CalculatePartA (calculatePartA)
import RunningCode (solveAv2, solvePuzzle)
import Solver (solver)

inputPathJason :: String
inputPathJason = "data/Day-22-INPUT.txt"

inputPathScott :: String
inputPathScott = "data/Day-22-input-ofd.txt"

solveIt :: (String, String) -> IO ()
solveIt twoInputFiles =
  hspec (specWith twoInputFiles)

specWith :: (String, String) -> Spec
specWith (inputTextJason, inputTextScott) = do
  describe "Puzzle Answers" $ do
    context "for Jason's exercise" $ do
      it "Part A cube count" $ do
        solveAv2 inputTextJason
          `shouldBe`
            577205
      it "Part B cube count" $ do
        solvePuzzle inputTextJason
          `shouldBe`
            1197308251666843
    let (partA, partB) = solver inputTextScott
    context "for Scott's exercise" $ do
      it "Part A cube count" $ do
        partA
          `shouldBe`
            598616
      it "Part B cube count" $ do
        partB
          `shouldBe`
            1193043154475246

