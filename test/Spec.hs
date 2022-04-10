{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import qualified Data.Text as T

import Day22 (Cuboid(..), Segment(..), RebootOperator(Sum), RebootStep(..), Segment, parseLine, x, y, z)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Parsing input line" $ do
    context "parseLine for '+'" $ do
      it "directly check the 'RebootStep' function" $ do
        actual `shouldBe`
          RebootStep (Sum, Cuboid {x = Segment (-20, 26), y = Segment (- 36, 17), z = Segment (- 47, 7)})
      it "should return the reboot step with function sum" $ do
        show actual `shouldBe`
          "RebootStep (Sum,Cuboid {x = (-20,26), y = (-36,17), z = (-47,7)})"
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

