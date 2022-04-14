module Day22 where

import Cuboid (Cuboid(..), parseCuboid, volume)
import RebootStep (generateRemnant, parseInputText)
import Remnant (Remnant, augment, reduce)

solvePuzzle :: String -> IO ()
solvePuzzle filePath = do
   inputText <- readFile filePath
   print (parseInputText inputText)


solver :: String -> Int
solver inputText =
  sum $ map volume $ generateRemnant $ parseInputText inputText
