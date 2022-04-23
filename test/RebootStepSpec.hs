{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore -}

module RebootStepSpec where

import Test.Hspec
import qualified Data.Text as T

import Cuboid (Source(..))
import Segment(SrcSeg(..))
import RebootStep(RebootStep(..), RebootOperator(..), parseLine)

actualRebootStepFor :: String -> RebootStep
actualRebootStepFor combiner = parseLine $ T.pack $ combiner ++ " x=-20..26,y=-36..17,z=-47..7"

rebootStepSpec =
  describe "parseLine" $ do
    context "with 'off' combiner" $ do
      it "directly checks the 'RebootStep' constructor for reduce" $ do
        (actualRebootStepFor "off") `shouldBe` -- ignore hlint
          RebootStep (Reduce, Source [SrcSeg (-20, 26), SrcSeg (- 36, 17), SrcSeg (- 47, 7)])
    context "with 'on' combiner" $ do
      it "directly checks the 'RebootStep' constructor for augment" $ do
        (actualRebootStepFor "on") `shouldBe` -- ignore hlint
          RebootStep (Augment, Source [SrcSeg (-20, 26), SrcSeg (-36, 17), SrcSeg (-47, 7)])
      it "returns the reboot step with function augment" $ do
        (show $ actualRebootStepFor "on") `shouldBe` -- ignore hlint
          "RebootStep (Augment,Source [(-20,26),(-36,17),(-47,7)])"
