{-
    2022-03-22
    . Advent of Code - Day 22 - Puzzle 1 of 2
      --- Day 22: Reactor Reboot ---
      flipping cuboids on/off based on reboot steps:
        on x=10..12,y=10..12,z=10..12
        on x=11..13,y=11..13,z=11..13
        off x=9..11,y=9..11,z=9..11
        on x=10..10,y=10..10,z=10..10

      The initialization procedure only uses cubes that
      have x, y, and z positions of at least -50 and at
      most 50. For now, ignore cubes outside this region.

      Execute the reboot steps. Afterward, considering only
      cubes in the region x=-50..50,y=-50..50,z=-50..50,
      how many cubes are on?

      Part B - forget the region. How many cubes are on?!!!
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

import Data.Char ( isDigit )
-- import Data.List ( maximumBy, minimumBy, group, sort )
import Data.List ( sort, sortBy )
import Data.Function (on)
-- import qualified Data.Map.Strict as Map
import qualified Data.Set as S


-- data PwrStep1 a b = (Num a, Num b) => (Char, [(a, b)], XYZ, LWH) deriving (Show, Ord)
data PwrStep2 a = PwrStep { onOff::Char, dimsXYZ::[(a,a)], xyz::XYZ, lwh::LWH }

-- mySort :: Ord b => [(a, b)] -> [(a, b)]
-- mySort = sortBy (flip compare `on` snd)

-- :: (Num a, Num b) => (Char, [(a, b)], XYZ, LWH)
-- ::  Num a         => (Char, [(a, a)], XYZ, LWH)
-- mySort (_, this, _, _) = sortBy (compare `on` this)
mySort :: (Num a, Ord a) => [(Char, [(a, a)], XYZ, LWH)] -> [(Char, [(a, a)], XYZ, LWH)]
mySort = sortBy (compare `on` this) -- flip compare sorts in reverse order
 where
   this (_,here,_,_) = fst $ head here
   this _ = error "Invalid Pattern Match!"

inputTest = "Day-22-INPUT-test.txt"
inputReal = "Day-22-INPUT.txt"

-- toIntList = map (\x -> read x :: Int) . words

tuplify2 :: [a] -> (a,a)
tuplify2 = \case
  [x,y] -> (x,y)
  _ -> error "List length /= 2"

tuplify6to3pair :: [a] -> [(a,a)]
tuplify6to3pair = \case
  [x0,x1,y0,y1,z0,z1] -> [(x0,x1),(y0,y1),(z0,z1)]
  _ -> error "List length /= 6"

isPosOrNeg :: Char -> Bool
isPosOrNeg c = isDigit c || (c == '-')

numCharOrSpc :: Char -> Char
numCharOrSpc c = if isPosOrNeg c then c else ' '

-- splitXYZ :: String -> [String]
-- splitXYZ :: String -> [Int]
-- splitXYZ s = words $ map (\c -> if c == ',' then ' ' else c) s
-- splitXYZ s = words $ map (flip if' ' ' =<< ((',') ==)) s -- per pointfree.io, but ... (if') ?
-- splitXYZ s = words $ words $ map spc4comma s
  -- where
  --   spc4comma ',' = ' '
  --   spc4comma  c  =  c
splitXYZ s = tuplify6to3pair $ map (\s -> read s :: Int)
             $ words $ map numCharOrSpc s

-- keepFstAltSnd :: (String, String) -> (String, [String])
-- keepFstAltSnd :: (String, String) -> (String, [Int])
keepFstAltSnd (f,s) = (f, splitXYZ s)

keepPlusMinus50 (_,xyzRanges) = foldr inRange True xyzRanges
  where
    inRange (low, high) valid = low `elem` [-50..50] && high `elem` [-50..50] && valid

buildCubesList (f, s) = (f, cubesList)
  where
    cubesList = [[x,y,z] | x<-[x0..x1],y<-[y0..y1],z<-[z0..z1]]
    [(x0,x1),(y0,y1),(z0,z1)] = s

-- solveA :: Int -> String -> [(String, String)] -> Int
solveA fileData =
  let lns      = lines fileData
      steps    = map (keepFstAltSnd . tuplify2 . words) lns
      pm50     = filter keepPlusMinus50 steps
      cubesLst = map buildCubesList pm50
  in
      length $ foldl setify S.empty cubesLst
        where
          setify accuSet (cmd, cLst) = case cmd of
            "on"  -> S.union accuSet (S.fromList cLst)
            "off" -> S.difference accuSet (S.fromList cLst)
            _     -> error "cmd /= on/off"

-- the following worked up until solveA returned pm50
-- da d = putStrLn $ unlines $ map show $ solveA d
{-
    ==============================
      *** VERY NAIVE ATTEMPT ***
    > solveB d
    Killed
    ==============================
-}

solveBv1 fileData =
  let lns      = lines fileData
      steps    = map (keepFstAltSnd . tuplify2 . words) lns
      -- pm50     = filter keepPlusMinus50 steps
      cubesLst = map buildCubesList steps -- pm50
  in
      length $ foldl setify S.empty cubesLst
        where
          setify accuSet (cmd, cLst) = case cmd of
            "on"  -> S.union accuSet (S.fromList cLst)
            "off" -> S.difference accuSet (S.fromList cLst)
            _     -> error "cmd /= on/off"

newtype XYZ = XYZ (Int, Int, Int) deriving (Show)
data LWH = LWH { xDim::Int, yDim::Int, zDim::Int } deriving (Show)

-- keepFstAltSndLWH (f,s) = (onOff, xyzs, (minXYZ, dims))
keepFstAltSndLWH (f,s) = (onOff, xyzs, xyzT, lwhRT)
  where
    (_, xyzs)     = keepFstAltSnd (f, s)
    dims          = map dist xyzs
    dist (n0, n1) = abs(n0-n1)+1
    lwhRT         = LWH (dims !! 0) (dims !! 1) (dims !! 2)
    minXYZ        = map fst xyzs
    xyzT          = XYZ (minXYZ !! 0, minXYZ !! 1, minXYZ !! 2)
    onOff         = case f of
      "on"  -> '+'
      "off" -> '-'
      _     -> error "not 'on' or 'off'!"

solveBv2 fileData =
  let lns      = lines fileData
      steps    = map (keepFstAltSndLWH . tuplify2 . words) lns
      -- pm50     = filter keepPlusMinus50 steps
      -- cubesLst = map buildCubesList steps -- pm50
  in
      steps
      -- length $ foldl setify S.empty cubesLst
      --   where
      --     setify accuSet (cmd, cLst) = case cmd of
      --       "on"  -> S.union accuSet (S.fromList cLst)
      --       "off" -> S.difference accuSet (S.fromList cLst)
      --       _     -> error "cmd /= on/off"

-- the following works while solveBv2 returns a List of things
db d = putStrLn $ unlines $ map show $ solveBv2 d

{-  
    > d <- readFile inputTest
    > lines d !! 0
    "on x=-20..26,y=-36..17,z=-47..7"

    > l0 = lines d !! 0
          -- OBSOLETE
          > takeWhile (/= ' ') l0
          "on"
          > tail $ dropWhile (/= ' ') l0
          "x=-20..26,y=-36..17,z=-47..7"

    > words l0
    ["on","x=-20..26,y=-36..17,z=-47..7"]

    > rangesStr = words l0 !! 1
          - considering ... is this more efficient?
          > break (== ',') rangesStr
          ("x=-20..26",",y=-36..17,z=-47..7")

    > words $ map (\c -> if c == ',' then ' ' else c) rangesStr
    ["x=-20..26","y=-36..17","z=-47..7"]

    > xRangeStr = flip (!!) 0
                  $ words $ map (\c -> if c == ',' then ' ' else c) rangesStr

    > xRange = words $ map (\c -> if (isNumber c) || (c == '-') then c else ' ') xRangeStr
    ["-20","26"]

          -- unnecessary?!
          > read "-42"   :: Int
          -42
          > read " -42"  :: Int
          -42
          > read " -42 " :: Int
          -42

    > map (\s -> read s :: Int) xRange
    [-20,26]
    
    > putStrLn $ unlines $ map (show) $ [(x,y,z) | x<-[10..12],y<-[10..12],z<-[10..12]]
    output is the 27 (x,y,z) cubes

              Data.Set
              --------
              union :: Ord a => Set a -> Set a -> Set a Source #

              O(m*log(n/m + 1)), m <= n. The union of two sets, preferring
              the first set when equal elements are encountered.

              difference :: Ord a => Set a -> Set a -> Set a Source #

              O(m*log(n/m + 1)), m <= n. Difference of two sets.

              Return elements of the first set not existing in the second set.

              difference (fromList [5, 3]) (fromList [5, 7]) == singleton 3

    > solveA ...
    should == 590784  -- cubes "on" inside region:
                      -- x=-50..50,y=-50..50,z=-50..50
-}