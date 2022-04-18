{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cuboid where

import qualified Data.Text as T

import Segment (Segment(..), SrcSeg(..), TrgSeg(..), toSrcSeg)

data Source = Source
  { x :: SrcSeg
  , y :: SrcSeg
  , z :: SrcSeg
  } deriving (Eq, Show)

data Target = Target
  { x :: TrgSeg
  , y :: TrgSeg
  , z :: TrgSeg
  } deriving (Eq, Show)

volume :: Target -> Int
volume c = 0 -- TODO

{- "x=-20..26,y=-36..17,z=-47..7" -}
parseSource :: T.Text -> Source
parseSource str =
  let
    xyz = T.splitOn "," str
    [x,y,z] = map (toSrcSeg . last . T.splitOn "=") xyz
  in
    Source { x, y, z }
