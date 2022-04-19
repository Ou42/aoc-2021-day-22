{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cuboid where

import qualified Data.Text as T

import Segment (Segment(..), SrcSeg(..), TrgSeg(..), convert, toSrcSeg)

data Source = Source
  { xSrc :: SrcSeg
  , ySrc :: SrcSeg
  , zSrc :: SrcSeg
  } deriving (Eq, Show)

data Target = Target
  { x :: TrgSeg
  , y :: TrgSeg
  , z :: TrgSeg
  } deriving (Eq, Show)

convertFromSourceToTarget :: Source -> Target
convertFromSourceToTarget source =
  Target
    { x = convert ( xSrc source )
    , y = convert ( ySrc source )
    , z = convert ( zSrc source )
    }


volume :: Target -> Int
volume c = 0 -- TODO

{- "x=-20..26,y=-36..17,z=-47..7" -}
parseSource :: T.Text -> Source
parseSource str =
  let
    xyz = T.splitOn "," str
    [xSrc,ySrc,zSrc] = map (toSrcSeg . last . T.splitOn "=") xyz
  in
    Source { xSrc, ySrc, zSrc }
