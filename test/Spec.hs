module Main where

import Test.Hspec
import qualified Data.Text as T

import CuboidSpec()
import RebootStepSpec (rebootStepSpec)
import SegmentSpec (segmentSpec)
import SegmentTestSpec (segmentTestSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  rebootStepSpec
  -- cuboidSpec
  segmentSpec
  -- segmentTestSpec