{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore -}

module Main where

import Test.Hspec
import qualified Data.Text as T

import Cuboid(Cuboid(..), nextSegment)
import Segment(Segment(..))
import RebootStep(RebootStep(..), RebootOperator(..), parseLine)
import SegmentSpec

main :: IO ()
main = hspec spec

actualRebootStepFor :: String -> RebootStep
actualRebootStepFor combiner = parseLine $ T.pack $ combiner ++ " x=-20..26,y=-36..17,z=-47..7"

spec :: Spec
spec = do
  describe "parseLine" $ do
    context "with 'off' combiner" $ do
      it "directly checks the 'RebootStep' constructor for reduce" $ do
        (actualRebootStepFor "off") `shouldBe` -- ignore hlint
          RebootStep (Reduce, Cuboid {x = Segment (-20, 26), y = Segment (- 36, 17), z = Segment (- 47, 7)})
    context "with 'on' combiner" $ do
      it "directly checks the 'RebootStep' constructor for augment" $ do
        (actualRebootStepFor "on") `shouldBe` -- ignore hlint
          RebootStep (Augment, Cuboid {x = Segment (-20, 26), y = Segment (-36, 17), z = Segment (-47, 7)})
      it "returns the reboot step with function augment" $ do
        (show $ actualRebootStepFor "on") `shouldBe` -- ignore hlint
          "RebootStep (Augment,Cuboid {x = (-20,26), y = (-36,17), z = (-47,7)})"
  describe "Cuboid functionality" $ do
    let c = Cuboid { x = Segment (1, 2), y = Segment (3, 4), z = Segment (5, 6)  }
    context "next segment in cuboid" $ do
      it "returns y for x" $ do
        nextSegment (x c) c `shouldBe` (y c)
      it "returns z for y" $ do
        nextSegment (y c) c `shouldBe` (z c)
      it "returns y for x" $ do
        nextSegment (z c) c `shouldBe` (x c)
