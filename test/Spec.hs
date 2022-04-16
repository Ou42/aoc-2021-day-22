module Main where

import Test.Hspec
import qualified Data.Text as T

import CuboidSpec(cuboidSpec)
import RebootStepSpec (rebootStepSpec)
import SegmentSpec (segmentSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  rebootStepSpec
  cuboidSpec
  segmentSpec