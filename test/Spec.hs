{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import qualified Data.Text as T

import Cuboid(Cuboid(..))
import Segment(Segment(..))
import RebootStep(RebootStep(..), RebootOperator(..), parseLine)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Parsing input line" $ do
    context "parseLine for '+'" $ do
      it "directly check the 'RebootStep' function" $ do
        actual `shouldBe`
          RebootStep (Augment, Cuboid [Segment (-20, 26), Segment (- 36, 17), Segment (- 47, 7)])
      it "should return the reboot step with function augment" $ do
        show actual `shouldBe`
          "RebootStep (Augment,Cuboid [(-20,26),(-36,17),(-47,7)])"
    where actual = parseLine (T.pack "on x=-20..26,y=-36..17,z=-47..7")

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

