{-# LANGUAGE OverloadedStrings #-}

module Day22 where

import qualified Data.Text as T

import Cuboid (Cuboid(..), parseCuboid, volume)
import Remnant (Remnant, augment, reduce)

solvePuzzle :: String -> IO ()
solvePuzzle filePath = do
   inputText <- readFile filePath
   print (parseInputText inputText)


solver :: String -> Int
solver inputText =
  sum $ map volume $ generateRemnant $ parseInputText inputText

{- | Parse the exercise's input file contents to a list of reboot steps.
   |
   | Example:
   |
   | rebootSteps =
   |   parseInputFile "data/Day-22-Input-test.txt"
-}
parseInputText :: [Char] -> [RebootStep]
parseInputText inputText =
   map (parseLine . T.pack) $ lines inputText

{-
   "on x=-20..26,y=-36..17,z=-47..7"
-}
parseLine :: T.Text -> RebootStep
parseLine line =
   mapTopTuple $ T.splitOn " " line
   where
      mapTopTuple :: [T.Text] -> RebootStep
      mapTopTuple (combine:(cuboidStr:(remainder))) =
         RebootStep ( if combine == "on" then Sum else Difference
                    , parseCuboid cuboidStr
                    )

generateRemnant :: [RebootStep] -> Remnant
generateRemnant rbs = [] -- TODO

{- Selects the function to implement the combining of the source and target cuboids
-}
data RebootOperator = Sum | Difference deriving (Eq, Show)

{- | The information parsed from each line in the Day-22 exercise input file
 -}
newtype RebootStep = RebootStep (RebootOperator,Cuboid) deriving (Eq, Show)

{- | Applies a `RebootStep` to a target `Cuboid`
   |
   | Properties
   |
   | 1. The `rebootStep` must always precede the `target`'s `RebootStep` in the `RebootSteps`' list.
-}
applyReboot :: RebootStep -> Cuboid -> Remnant
applyReboot rebootStep target = [] -- TODO
