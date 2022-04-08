{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Day22 where

{-
    2022-03-22 -> 2022-04-01
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

import Data.Char ( isDigit )
import Data.List ( sort, sortBy, groupBy, intersect
                 , intersperse, intercalate, tails )
import Data.Function (on)
import Data.Maybe (catMaybes, isNothing, maybe)
import qualified Data.Set as S
import Data.List.Split (chunksOf)

inputTest :: [Char]
inputTest = "data/Day-22-INPUT-test.txt"
inputReal :: [Char]
inputReal = "data/Day-22-INPUT.txt"
i3 :: [Char]
i3 = "data/i3.txt"

{-
    Ideas:
      (1) perhaps, jump to first "off" step
          and then work backwards? potentially less work,
          if an "on" is entirely w/in the "off" bounding box.
      (2) as it stands, takeWhile "on" then build up until
          an "off". Will need various levels of intersection
          checks. Bounding box etc.
          + maybe create a sliceAt func that takes a plane
            and returns 2 rectPrisms ...
          + ... or ... just #3
      (3) sort the data and make "slices" == new rectPrisms
          so that intersections become their own rectPrisms
          and getting the # of "on" cubes == sum of vol of
          all non-overlapping rectPrisms (where vol = l*w*h)
      (4) although still need to 'deal' w/ intersections (dupe)
          cubes, rather than cycle thru entire "off", could
          check if *existing* "on" cubes are "inside" "off"
          keep the # removed, subtract from retPrism's vol
          ... but what about mutiple "off" cubes?!
      (5) having difficulty chopping up a cuboid into parts
          ... but union func only needs to chop up ONE cuboid?!
          ... *therefore* NO sorting?!
          > union2 cuboidA cuboidB ==
              List of cuboidA parts *NOT* including over-lap
                ++ [cuboidB] unedited.

-}

newtype XYZ = XYZ (Int, Int, Int) deriving (Show)
data LWH = LWH { xDim::Int, yDim::Int, zDim::Int } deriving (Show)

type Rng a      = (a,a)
type Cuboid a   = [Rng a]
-- 'PwrStep4 a' doesn't req {-# LANGUAGE RankNTypes #-}
type PwrStep4 a = (Char, [Rng a], XYZ, LWH)
type PwrStep5 a = (Char, [Rng a])

-- toIntList = map (\x -> read x :: Int) . words

pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

intersectUsingSets :: (Num a, Ord a, Enum a) => Cuboid a -> Cuboid a -> Bool
--intersectUsingSets :: (Num a, Ord a, Enum a) => [Rng a] -> [Rng a] -> Bool
intersectUsingSets cuA@[ax,ay,az] cuB@[bx,by,bz] =
  let ixs = S.intersection (S.fromList [fst ax..snd ax]) (S.fromList [fst bx..snd bx])
      iys = S.intersection (S.fromList [fst ay..snd ay]) (S.fromList [fst by..snd by])
      izs = S.intersection (S.fromList [fst az..snd az]) (S.fromList [fst bz..snd bz])
  in not $ null ixs || null iys || null izs
--  in not (null ixs) && not (null iys) && not (null izs)
--  in length ixs > 0 && length iys > 0 && length izs > 0
intersectUsingSets _ _ = error "Wrong # of ranges!"

-- intersectUsingSets2 :: (Num a, Ord a, Enum a) => [Rng a] -> [Rng a] -> (Bool, [Rng a])
intersectUsingSets2 :: (Num a, Ord a, Enum a) => Cuboid a -> Cuboid a -> (Bool, Cuboid a)
intersectUsingSets2 cuA@[ax,ay,az] cuB@[bx,by,bz] =
  let ixs = S.intersection (S.fromList [fst ax..snd ax]) (S.fromList [fst bx..snd bx])
      iys = S.intersection (S.fromList [fst ay..snd ay]) (S.fromList [fst by..snd by])
      izs = S.intersection (S.fromList [fst az..snd az]) (S.fromList [fst bz..snd bz])
      hasIntersection = not $ null ixs || null iys || null izs
      nxs = (S.findMin ixs, S.findMax ixs)
      nys = (S.findMin iys, S.findMax iys)
      nzs = (S.findMin izs, S.findMax izs)
  in (hasIntersection, if hasIntersection then [nxs, nys, nzs] else [])
--  in (not $ null ixs || null iys || null izs, [nxs, nys, nzs])
--  in not (null ixs) && not (null iys) && not (null izs)
--  in length ixs > 0 && length iys > 0 && length izs > 0

intersectUsingSets2 _ _ = error "Wrong # of ranges!"

-- diffUsingSets :: (Num a, Ord a, Enum a) => [Rng a] -> [Rng a] -> [[Rng a]]
diffUsingSets :: (Num a, Ord a, Enum a) => Cuboid a -> Cuboid a -> [Cuboid a]
diffUsingSets cuA@[] cuB = []
diffUsingSets cuA@[(axMn,axMx),(ayMn,ayMx),(azMn,azMx)] cuB =
  let (hasIntersection, intersection) = intersectUsingSets2 cuA cuB
      [iax,iay,iaz] = intersection
      (iaxMn, iaxMx) = iax
      (iayMn, iayMx) = iay
      (iazMn, iazMx) = iaz
      dxsLT = if axMn  < iaxMn then [(axMn,iaxMn-1), (ayMn,ayMx),    (azMn,azMx)] else []
      dxsGT = if iaxMx < axMx  then [(iaxMx+1,axMx), (ayMn,ayMx),    (azMn,azMx)] else []
      dysLT = if ayMn  < iayMn then [iax,            (ayMn,iayMn-1), (azMn,azMx)] else []
      dysGT = if iayMx < ayMx  then [iax,            (iayMx+1,ayMx), (azMn,azMx)] else []
      dzsLT = if azMn  < iazMn then [iax,           iay,          (azMn,iazMn-1)] else []
      dzsGT = if iazMx < azMx  then [iax,           iay,          (iazMx+1,azMx)] else []
      diffs = filter (not . null) [dxsLT, dxsGT, dysLT, dysGT, dzsLT, dzsGT]
  in if hasIntersection then diffs else []
diffUsingSets _ _ = error "Wrong # of ranges!"

-- add2Cuboids :: (Num a, Ord a, Enum a) => [Rng a] -> [Rng a] -> [[Rng a]]
add2Cuboids :: (Num a, Ord a, Enum a) => Cuboid a -> Cuboid a -> [Cuboid a]
add2Cuboids [] cuB  = [cuB]
add2Cuboids cuA cuB = cuB : diffUsingSets cuA cuB

-- addCuboids :: (Num a, Ord a, Enum a) => [[Rng a]] -> [Rng a] -> [[Rng a]]
addCuboids :: (Num a, Ord a, Enum a) => [Cuboid a] -> Cuboid a -> [Cuboid a]
-- addCuboids cuLst cub = concatMap (`diffUsingSets` cub) cuLst
addCuboids cuLst cuB = concatMap (\cuA -> diffUsingSets cuA cuB) cuLst -- didn't change anything!

-- runAddOnCLst :: (Num a, Ord a, Enum a) => [[Rng a]] -> [[Rng a]]
runAddOnCLst :: (Num a, Ord a, Enum a) => [Cuboid a] -> [Cuboid a]
runAddOnCLst cuLst = foldl (\accu cuA -> cuA : (addCuboids accu cuA) ++ tail accu)
                       [head cuLst]
                       (tail cuLst)
                          --  -> (add2Cuboids (head accu) cuA) ++ tail accu)
                          --  [head cuLst]
                          --  (tail cuLst)

iFreeProbs cuLst = foldl (\accu (cu:cus) -> (map (\cu2 -> if (intersectUsingSets cu cu2)
                                                            then [42]
                                                            else []) cus):accu) [] ((init . tails) cuLst)

iFreeProbs2 cuLst = foldl (\accu (cu:cus) -> (map (\cu2 -> if (intersectUsingSets cu cu2)
                                                            then [cu,cu2]
                                                            else []) cus):accu) [] ((init . tails) cuLst)

iFreeAllLsts cuLst = foldl (\accu (cu:cus) -> (map (intersectUsingSets cu) cus):accu) [] ((init . tails) cuLst)
iFreeAll cuLst = all (==False) $ concat $ iFreeAllLsts cuLst

runAddOnCLstDebug :: (Num a, Ord a, Enum a, Show a) => [Cuboid a] -> [Cuboid a]
runAddOnCLstDebug cuLst = foldl go [head cuLst] (tail cuLst)
  where
    addOp accu' cuA' = cuA' : (addCuboids accu' cuA') ++ tail accu'
    -- go    accu cuA   = if iFreeAll (addOp accu cuA) then addOp accu cuA else error "DANGER! DANGER!"
    -- go    accu cuA   = if iFreeAll (addOp accu cuA) then addOp accu cuA else error ("DANGER! DANGER!\n" ++ un (addOp accu cuA))
    -- go    accu cuA   = if iFreeAll (addOp accu cuA) then addOp accu cuA else error ("DANGER! DANGER!\n" ++ show (addOp accu cuA))
    go    accu cuA   =
      let res = (addOp accu cuA)
      in if iFreeAll res then res else error (show res)


sortOnXmin :: (Num a, Ord a) => [PwrStep5 a] -> [PwrStep5 a]
sortOnXmin = sortBy (compare `on` this) -- (flip compare ...) sorts in descending order
  where
    this (_,here) = fst $ head here

sortXYZs :: Ord a => Int -> [[(a,a)]] -> [[(a,a)]]
sortXYZs 3   = error "Index == 3. Can't sort!"
sortXYZs idx = sortBy (compare `on` (fst . (!! idx))) -- (flip compare ...) sorts in descending order

bbox :: Ord a => [(a, a)] -> [(a, a)] -> [(a, a)]
bbox = zipWith hmm
  where
    hmm :: (Ord a) => (a, a) -> (a, a) -> (a, a)
    hmm (t0fst, t02nd) (t1fst, t1snd) = (min t0fst t1fst, max t02nd t1snd)

insideBBox :: (Ord a, Enum a) => [(a, a)] -> [(a, a)] -> Bool
insideBBox bbox rPrism =
  ox0 `elem` [bx0..bx1] && ox1 `elem` [bx0..bx1] &&
  oy0 `elem` [by0..by1] && oy1 `elem` [by0..by1] &&
  oz0 `elem` [bz0..bz1] && oz1 `elem` [bz0..bz1]
  where
    [(bx0,bx1),(by0,by1),(bz0,bz1)] = bbox
    [(ox0,ox1),(oy0,oy1),(oz0,oz1)] = rPrism


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

xyzStrToTup :: [Char] -> [(Int, Int)]
xyzStrToTup s = tuplify6to3pair $ map (\s -> read s :: Int)
                $ words $ map numCharOrSpc s

keepFstAltSnd :: ([Char], [Char]) -> (Char, [(Int, Int)])
keepFstAltSnd (f,s) = (onOff, xyzStrToTup s)
  where
    onOff = case f of
      "on"  -> '+'
      "off" -> '-'
      _     -> error "not 'on' or 'off'!"


keepPlusMinus50 :: (Foldable t, Eq a1, Eq a2, Num a1, Num a2, Enum a1, Enum a2) => (a3, t (a1, a2)) -> Bool
keepPlusMinus50 (_,xyzRanges) = foldr inRange True xyzRanges
  where
    inRange (low, high) valid = low `elem` [-50..50] && high `elem` [-50..50] && valid

buildCubesList :: Enum a1 => (a2, [(a1, a1)]) -> (a2, [[a1]])
buildCubesList (f, s) = (f, cubesList)
  where
    cubesList = [[x,y,z] | x<-[x0..x1],y<-[y0..y1],z<-[z0..z1]]
    [(x0,x1),(y0,y1),(z0,z1)] = s

-- works: d <- readFile inputTest && solveA d == 590784
-- works: d <- readFile inputReal && solveA d == 577205
solveA :: [Char] -> Int
solveA fileData =
  let lns      = lines fileData
      steps    = map (keepFstAltSnd . tuplify2 . words) lns
      pm50     = filter keepPlusMinus50 steps
      cubesLst = map buildCubesList pm50
  in
      length $ foldl setify S.empty cubesLst

setify :: Ord a => S.Set a -> (Char, [a]) -> S.Set a
setify accuSet (cmd, cLst) = case cmd of
  '+' -> S.union accuSet (S.fromList cLst)
  '-' -> S.difference accuSet (S.fromList cLst)
  _   -> error "cmd /= on/off"

-- doesn't finish. Linux "kills" it.
solveBv1 :: [Char] -> Int
solveBv1 fileData =
  let lns      = lines fileData
      steps    = map (keepFstAltSnd . tuplify2 . words) lns
      cubesLst = map buildCubesList steps
  in
      length $ foldl setify S.empty cubesLst

-- iFree == intersection Free?
-- iFree :: (Num a, Ord a) => PwrStep5 a -> [PwrStep5 a] -> Bool
iFree :: Foldable t1 => (t2 -> a -> Bool) -> t2 -> t1 a -> Bool
iFree intersectFunc cuA cuLst = not $ any ((== True) . intersectFunc cuA) cuLst
-- iFree _ _ = error "List of cuboids empty?! or length 1?!"

-- culprits :: (Num a, Ord a) => PwrStep5 a -> [PwrStep5 a] -> [PwrStep5 a]
culprits :: (t -> a -> Bool) -> t -> [a] -> [a]
culprits intersectFunc pwrStepA = filter (intersectFunc pwrStepA)

getDiffGrps (h:t) =
  fst
  $ foldl (\(accu,cuA) cuB -> (accu ++ [difference intersects cuA cuB],cuB)) ([],h) t
getDiffGrps _ = error "List of cuboids empty?! or length 1?!"

-- goDiffs = concat getDiffGrps
goDiffs :: [(Char, [Rng Int])] -> [(Char, [Rng Int])]
goDiffs cuboidLst = concat (getDiffGrps cuboidLst) ++ [last cuboidLst]

{-
  * -- unionOnGrp (previously newDiffGrps):
  * --        a UNION of "on" ('+') PwrSteps
  * -- given:
  * --        an intersecion function
  * --        & a list of steps: pwrStepLst -- should ALL be same on/off
  * -- take head & tail:      (h:t) = pwrStepLst
  * -- take head and check if iFree ( intesects anything )
  *      -- is h iFree? ( no intersections ) Yes: save it, recurse on t
  *      --   else, take difference of h with head of culprits
  *      --         this makes a new list of PwrSteps
  *      --         recurse on this prepended to remaining PwrSteps
  *
  *********************************************************************** -}

unionOnGrp :: (Enum a, Ord a, Num a) =>
                        (PwrStep5 a -> PwrStep5 a -> Bool)
                        -> [PwrStep5 a]
                        -> [PwrStep5 a]
unionOnGrp _ []    = error "EMPTY!! Power Step List!"
unionOnGrp _ [psA] = [psA]
unionOnGrp intersectFunc pwrStepLst@(psA:psRest) =
  let fstCulprit = (head $ culprits intersectFunc psA psRest)
      diff       = difference intersectFunc psA fstCulprit
  in
      if iFree intersectFunc psA psRest
        then psA : unionOnGrp intersectFunc psRest
        else unionOnGrp intersectFunc (diff ++ psRest)

-- diffOnGrpVsOneOff :: (Enum a, Ord a, Num a) =>
--                        [PwrStep5 a]
--                        -> PwrStep5 a
--                        -> [PwrStep5 a]
diffOnGrpVsOneOff onGrp oneOff = -- error "???"
  concatMap (flip (difference intersects3) $ oneOff) onGrp

sa :: Int -> String -> String
sa i d = unlines $ take i $ lines d
-- sb1 :: Int -> String -> [PwrStep5 Int]
sb1 i d = unionOnGrp intersects $ take i $ allSteps d
vb1 :: Int -> String -> Int
vb1 i d = vol $ map snd $ sb1 i d
-- sb3 :: Int -> String -> [PwrStep5 Int]
sb3 i d = unionOnGrp intersects3 $ take i $ allSteps d
vb3 :: Int -> String -> Int
vb3 i d = vol $ map snd $ sb3 i d

solveBv3 fileData =
  let as = allSteps fileData
      grps = gb as
  in
    vol $ map snd
      $ unionOnGrp intersects3 $ as

allSteps :: [Char] -> [(Char, [(Int, Int)])]
allSteps fileData = steps
  where
    lns   = lines fileData
    steps = map (keepFstAltSnd . tuplify2 . words) lns

appendXYZLWH :: (a, [(Int, Int)]) -> (a, [(Int, Int)], XYZ, LWH)
appendXYZLWH (onOff, xyzs) = (onOff, xyzs, xyzMinPt, lwh)
  where
    minXYZ   = map fst xyzs
    xyzMinPt = XYZ (head minXYZ, minXYZ !! 1, minXYZ !! 2) -- hlint: use head
    dims     = map dist xyzs
    dist (n0, n1)
             = abs(n0-n1)+1
    lwh      = LWH (head dims) (dims !! 1) (dims !! 2) -- hlint: use head

-- display steps
ds :: Show a => [a] -> IO ()
ds d = putStrLn $ unlines $ map show d

un d = unlines $ map show d

-- takeWhile "on"
twOn :: [(Char, b)] -> [(Char, b)]
twOn = takeWhile (\(onOff,_) -> onOff == '+')

-- groupBy on/off
gb :: [PwrStep5 a] -> [[PwrStep5 a]]
gb = groupBy (\a b -> fst a == fst b)

replace :: Int -> [a] -> a -> [a]
replace idx' rng axisRng =
  take idx' rng ++ [axisRng] ++ drop (idx'+1) rng

difference :: (Enum a, Ord a, Num a) => (PwrStep5 a -> PwrStep5 a -> Bool) -> PwrStep5 a -> PwrStep5 a -> [PwrStep5 a]
difference intersectFunc pwrStepA@(cmdA, rngA) pwrStepB@(_, rngB) =
  let
    mbX   = myBreak (head rngA) (head rngB)
    xDiff = catMaybes $ head mbX : [last mbX]
    xUpdt = map (replace 0 rngA) xDiff
    xEQ   = mbX !! 1 -- <--- this is a Just (Int, Int) or a Nothing!!!
    rAxEQ = maybe rngA (replace 0 rngA) xEQ
      {- hlint: Use maybe
         Found:
           if isNothing xEQ then rngA else replace 0 rngA (fromJust xEQ)
         Why not:
           maybe rngA (replace 0 rngA) xEQ -}
    mbY   = myBreak (rAxEQ !! 1) (rngB !! 1)
    yDiff = catMaybes $ head mbY : [last mbY]
    yUpdt = map (replace 1 rAxEQ) yDiff
    yEQ   = mbY !! 1
    rAyEQ = maybe rAxEQ (replace 1 rAxEQ) yEQ
    mbZ   = myBreak (rAyEQ !! 2) (rngB !! 2)
    zDiff = catMaybes $ head mbZ : [last mbZ]
    zUpdt = map (replace 2 rAyEQ) zDiff
    -- the following may just be the intersection of the 2 cuboids!
    -- this didn't exist: zEQ   = mbZ !! 1 -- should it?!
    -- this as well:      rAzEQ = maybe rAyEQ (replace 2 rAyEQ) zEQ
  in
    -- if the following doesn't work, a less efficient way:
    -- The (nub) function removes duplicate elements from a list.
    -- if intersects pwrStepA pwrStepB
    if intersectFunc pwrStepA pwrStepB
      then map ((,) cmdA) $ xUpdt ++ yUpdt ++ zUpdt
      else [pwrStepA]
    -- only the LT & GT of each myBreak?!
    -- NO! for mbX, yes, LT & GT
    -- for mbY, "update" rngA's xRng to be mbX's EQ
    -- for mbZ, "update" updated!! rngA's yRng to be mbY's EQ

vol :: [[(Int, Int)]] -> Int
vol = sum . map (product . map (\(mn,mx) -> abs(mx-mn)+1))

-- toObjFile :: (Num a, Show a) => [PwrStep5 a] -> String
-- mkObjFile' pwrSteps = unlines $ concatMap (("o obj42"++) . verts' . snd) pwrSteps

-- verts = putStrLn $ (++) "\nv " $ concat $ concat $ intersperse ["\nv "] $ map (intersperse " ") d
-- hlint: why not intercalate?
-- verts :: [[String]] -> String
-- verts d = putStrLn $ (++) "\nv " $ concat $ intercalate ["\nv "] $ (map (intersperse " ") d
verts d =
  {- -- (++) "\nv "
     -- $ concat $ intercalate ["\nv "]
      -- $ map (intersperse " " . map show . lst2Tup2Lst3 . snd)
      -- $ map ( (intersperse " ") . lst2Tup2Lst3 . snd) $ allSteps d
      -- id
      -- $ map ( lst2Tup2Lst3 . snd ) $ allSteps d
  -}
  putStrLn
  $
  unlines $ map show $ concat $ (map . map) concat
  $ map ( (map ((intersperse " ") . (:)"v")) . lst2Tup2Lst3 . snd ) $ allSteps d

-- > d <- readFile i3
-- > as = allSteps d
-- > putStrLn $ unlines $ map (show . lst2Tup2Lst3 . snd) as
-- > :t map (show . lst2Tup2Lst3 . snd) as
-- map (show . lst2Tup2Lst3 . snd) as :: [String]
-- > :t map (lst2Tup2Lst3 . snd) as
-- map (lst2Tup2Lst3 . snd) as :: [[[Int]]]

-- > e = Data.List.Split.chunksOf 3 [1..42]
-- [[1,2,3],[4,5,6],[7,8,9],[10,11,12],[13,14,15],[16,17,18],[19,20,21],[22,23,24],[25,26,27],[28,29,30],[31,32,33],[34,35,36],[37,38,39],[40,41,42]]
-- > > map (map show) e
-- [["1","2","3"],["4","5","6"],["7","8","9"],["10","11","12"],["13","14","15"],["16","17","18"],["19","20","21"],["22","23","24"],["25","26","27"],["28","29","30"],["31","32","33"],["34","35","36"],["37","38","39"],["40","41","42"]]

lst2Tup2Lst3 :: (Integral a, Show a) => [Rng a] -> [[String]]
lst2Tup2Lst3 [(mnX, mxX), (mnY, mxY), (mnZ, mxZ)] =
  -- l3 ==> expand by 0.5 in both directions along each axis
  -- innerShow $ l3 [(mnX, mxX), (mnY, mxY), (mnZ, mxZ)]
  innerShow $ l3NoShift [(mnX, mxX), (mnY, mxY), (mnZ, mxZ)]
lst2Tup2Lst3 _ = error "Invalid input. Input /= [(a,b),(c,d),(e,f)]!!!"

lst2Tup2Lst3' :: Show a => [(a, a)] -> [[String]]
lst2Tup2Lst3' [(mnX, mxX), (mnY, mxY), (mnZ, mxZ)] =
  [[show x, show y, show z] | x <- [mnX, mxX]
                            , y <- [mnY, mxY]
                            , z <- [mnZ, mxZ]]
lst2Tup2Lst3' _ = error "Invalid input. Input /= [(a,b),(c,d),(e,f)]!!!"

l2 :: (Fractional a, Show a) => [(a, a)] -> [[String]]
l2 [(mnX, mxX), (mnY, mxY), (mnZ, mxZ)] =
  [[show x, show y, show z] | x <- [mnX - 0.5, mxX + 0.5]
                            , y <- [mnY - 0.5, mxY + 0.5]
                            , z <- [mnZ - 0.5, mxZ + 0.5]]
l2 _ = error "Invalid input. Input /= [(a,b),(c,d),(e,f)]!!!"

l3 :: (Integral a, Fractional b) => [(a, a)] -> [[b]]
l3 [(mnX, mxX), (mnY, mxY), (mnZ, mxZ)] =
  [[x, y, z] | x <- [fromIntegral mnX - 0.5, fromIntegral mxX + 0.5]
             , y <- [fromIntegral mnY - 0.5, fromIntegral mxY + 0.5]
             , z <- [fromIntegral mnZ - 0.5, fromIntegral mxZ + 0.5]]
l3 _ = error "Invalid input. Input /= [(a,b),(c,d),(e,f)]!!!"

l3NoShift [(mnX, mxX), (mnY, mxY), (mnZ, mxZ)] =
  [[x, y, z] | x <- [mnX, mxX]
             , y <- [mnY, mxY]
             , z <- [mnZ, mxZ]]
l3NoShift _ = error "Invalid input. Input /= [(a,b),(c,d),(e,f)]!!!"

innerShow :: Show a => [[a]] -> [[String]]
innerShow lst = (map . map) show lst

verts' [(mnX, mxX), (mnY, mxY), (mnZ, mxZ)] =
      -- concatMap (("f"++) . (++"\n"))
      -- show $ chunksOf 3 $ concatMap ((:)' ' . show)
      map (\(x,y,z) -> "f " ++ show x ++ " " ++ show y ++ " " ++ show z)
      [(mnX,mnY,mnZ), (mnX,mnY,mxZ), (mnX,mxY,mnZ), (mnX,mxY,mxZ)
      ,(mxX,mnY,mnZ), (mxX,mnY,mxZ), (mxX,mxY,mnZ), (mxX,mxY,mxZ)]
verts' _ = error "42!"

myBreak :: (Enum a, Ord a, Num a) => Rng a -> Rng a -> [Maybe (Rng a)]
myBreak (s1,e1) (s2,e2) = [lt,eq,gt]
  where
    lt = if s1 < s2 then Just (s1, s2-1) else Nothing
    eq = if s1 `elem` [s2..e2] || e1 `elem` [s2..e2]
           then Just (max s1 s2, min e1 e2)
           else Nothing
    gt = if e1 > e2 then Just (e2+1, e1) else Nothing

intersects :: (Num a, Ord a) => PwrStep5 a -> PwrStep5 a -> Bool
intersects pwrStepA@(_, cuA) pwrStepB@(_, cuB) = go
  where
    go = foldr isInsideOneDim True rngAB
    rngAB = zip cuA cuB
    isInsideOneDim :: (Num a, Ord a) => ((a,a),(a,a)) -> Bool -> Bool
    isInsideOneDim ((s1,e1),(s2,e2)) bAccu = -- True
      ((s2 <= s1 && s1 <= e2) || (s2 <= e1 && e1 <= e2))
      && bAccu

intersects2 pwrStepA@(_, cuA) pwrStepB@(_, cuB) = go
  where
    go = all (==True) chkXYZ
    rngAB  = zip cuA cuB
    chkXYZ = [s1 `elem` [s2..e2] || e1 `elem` [s2..e2] | ((s1,e1),(s2,e2)) <- rngAB]

intersects3 :: (Num a, Ord a, Enum a) => PwrStep5 a -> PwrStep5 a -> Bool
intersects3 pwrStepA@(_, cuA) pwrStepB@(_, cuB) = go
  where
    go = all (==True) chkXYZ
    rngAB  = zip cuA cuB
    chkXYZ = [  s1 `elem` [s2..e2] || e1 `elem` [s2..e2]
             || s2 `elem` [s1..e1] || e2 `elem` [s1..e1] | ((s1,e1),(s2,e2)) <- rngAB]

main :: IO ()
main = do
  d <- readFile i3
  let sai3 = solveA d
  putStrLn $ "solveA on (" ++ i3 ++ ") \t= " ++ show sai3

  let sb3i3 = solveBv3 d
  putStrLn $ "\"solveBv3\" on (" ++ i3 ++ ") \t= "
    ++ show sb3i3

{-
    > solveA (with inputReal)
    should == 590784  -- cubes "on" inside region:
                      -- x=-50..50,y=-50..50,z=-50..50
-}
