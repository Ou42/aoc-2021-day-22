{-# LANGUAGE OverloadedStrings #-}

module SolutionChoices where

import Test.Hspec

import CalculatePartA (calculatePartA, scottSolver)

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
    let (partA, partB) = scottSolver inputTextJason
    context "for Jason's exercise" $ do
      it "Part A cube count" $ do
        partA
          `shouldBe`
            577205
      it "Part B cube count" $ do
        partB
          `shouldBe`
            1197308251666843
    let (partA, partB) = scottSolver inputTextScott
    context "for Scott's exercise" $ do
      it "Part A cube count" $ do
        partA
          `shouldBe`
            598616
      it "Part B cube count" $ do
        partB
          `shouldBe`
            1193043154475246

