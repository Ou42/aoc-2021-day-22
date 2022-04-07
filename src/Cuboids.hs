module Cuboids where


data Operation = Operation
  { action :: Cuboid -> Cuboid -> [Cuboid]
  , source :: Cuboid
  , target :: Cuboid
  }

augment :: Cuboid -> Cuboid -> [ Cuboid ]
augment source target = [] -- TODO

reduce :: Cuboid -> Cuboid -> [ Cuboid ]
reduce source target = [] -- TODO

data Cuboid = Cuboid
  { x :: Range
  , y :: Range
  , z :: Range
  }

data Range = Range
  { from :: Int
  , to   :: Int
  }

