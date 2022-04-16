{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import qualified Data.Text as T

import Cuboid(Cuboid(..), nextSegment)
import Segment(Segment(..))
import RebootStep(RebootStep(..), RebootOperator(..), parseLine)

{- HLINT ignore -}

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



  -- describe "setAdd" $ do
  --   context "cuboids do not overlap" $
  --     it "should return list containing the two cuboids" $
  --       1 + 2 `shouldBe` 3

    -- addCuboids ( Cuboid (1,2) (3,4) (5,6)) (Cuboid (10,11) (12,13) (14,15))
    --   `shouldBe` [ Cuboid (1,2) (3,4) (5,6), Cuboid (10,11) (12,13) (14,15) ]
    -- context "larger cuboid completely contains other cuboid"
    --   it "should return list containing the larger cuboid only"
    --     addCuboids (Cuboid (2,3) (2,3) (2,3)) (Cuboid (1,4) (1,4) (1,4))
    --       `shouldBe` [ Cuboid (1,4) (1,4) (1,4 )]
    -- context "cuboids overlaps 2 edges"
    -- context "cuboids overlaps 3 edges"

