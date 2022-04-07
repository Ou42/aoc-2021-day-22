module Cuboids where

data Cuboid = Cuboid
  { x :: Range
  , y :: Range
  , z :: Range
  }

data Range = Range
  { from :: Int
  , to   :: Int
  }

