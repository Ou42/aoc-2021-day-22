{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cuboid where

import qualified Data.Text as T

import Segment (Segment(..), toSegment)

{- | consists of 3-element list for x, y, and z respectively
   |
   | We do this because the operations we want to perform on a cuboid
   | are the same regardless of which axis the cuboid is oriented.
-}
data Cuboid = Cuboid [Segment]
   deriving (Eq, Show)

volume :: Cuboid -> Int
volume c = 0 -- TODO

{- "x=-20..26,y=-36..17,z=-47..7" -}
parseCuboid :: T.Text -> Cuboid
parseCuboid str =
    Cuboid $ map (toSegment . last . T.splitOn "=") $ T.splitOn "," str
