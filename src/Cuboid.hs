{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cuboid where

import qualified Data.Text as T

import Segment (Segment(..), toSegment)

data Cuboid = Cuboid
  { x :: Segment
  , y :: Segment
  , z :: Segment
  } deriving (Eq, Show)

volume :: Cuboid -> Int
volume c = 0 -- TODO

{- I'm pretty sure I don't need to do this
nextSegment :: Segment -> Cuboid -> Segment
nextSegment seg cb
  | x cb == seg = y cb
  | y cb == seg = z cb
  | z cb == seg = x cb
-}

{- "x=-20..26,y=-36..17,z=-47..7" -}
parseCuboid :: T.Text -> Cuboid
parseCuboid str =
  let
    xyz = T.splitOn "," str
    [x,y,z] = map (toSegment . last . T.splitOn "=") xyz
  in
    Cuboid { x, y, z }
