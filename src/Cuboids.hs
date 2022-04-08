module Cuboids where

{- TODO - precisely comment this
-}
data Action = Operation
  { combine :: Cuboid -> Cuboid -> [Cuboid]
  , source :: Cuboid
  , target :: Cuboid
  }

{- TODO - precisely comment this
-}
sum :: Cuboid -> Cuboid -> [ Cuboid ]
sum source target = [] -- TODO

{- TODO - precisely comment this
-}
difference :: Cuboid -> Cuboid -> [ Cuboid ]
difference source target = [] -- TODO

data Cuboid = Cuboid
  { x :: Range
  , y :: Range
  , z :: Range
  }

data Range = Range
  { from :: Int
  , to   :: Int
  }

