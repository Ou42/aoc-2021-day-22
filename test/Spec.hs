module Main where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "setAdd" $ do
    context "cuboids do not overlap" $
      it "should return list containing the two cuboids" $
        1 + 2 `shouldBe` 3

    -- addCuboids ( Cuboid (1,2) (3,4) (5,6)) (Cuboid (10,11) (12,13) (14,15))
    --   `shouldBe` [ Cuboid (1,2) (3,4) (5,6), Cuboid (10,11) (12,13) (14,15) ]
    -- context "larger cuboid completely contains other cuboid"
    --   it "should return list containing the larger cuboid only"
    --     addCuboids (Cuboid (2,3) (2,3) (2,3)) (Cuboid (1,4) (1,4) (1,4))
    --       `shouldBe` [ Cuboid (1,4) (1,4) (1,4 )]
    -- context "cuboids overlaps 2 edges"
    -- context "cuboids overlaps 3 edges"

