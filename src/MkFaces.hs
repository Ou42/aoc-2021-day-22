module MkFaces where

{-
-- +---------------------------------+
-- | to run: > putStrLn $ mkFaces 10 |
-- +---------------------------------+
-}

mkFaces :: Integer -> String
mkFaces n = unlines $ map go [0..n-1]
  where
      go i =  -- o cu-01 ...
        --   ("o cu-" ++ (if 5 < 10 then "0" else "") ++ show (5+1) ++ "\n") ++
        --   ("o cu-" ++ (if i < 10 then "0" else "") ++ show (i+1) ++ "\n") ++
          (++) ("o cu-" ++ (if i < 9 then "0" else "") ++ show (i+1) ++ "\n")
          $
        --   unlines $ map (("o cu-" ++ show (i+1) ++ "\n")++) $
        --   unlines $ map ((++)"f " . intercalate " " . (map (show . (*8)))) faces
        --   unlines $ map ((++)"f " . unwords . map (show . (+8*i))) faces
        --   (++) "hello" $
          unlines $ map ((++)"f " . unwords . map (show . (+8*i))) faces

faces = [[1, 2, 4, 3]
        ,[6, 5, 7, 8]
        ,[1, 2, 6, 5]
        ,[3, 4, 8, 7]
        ,[1, 3, 7, 5]
        ,[2, 4, 8, 6]]

-- > map (map (*8)) faces
-- [[8,16,32,24],[40,48,56,64]...]
-- > map (map (show . (*8))) faces
-- [["8","16","32","24"],["40","48","56","64"]...]
-- Prelude Data.List> intersperse [42] faces
-- [[1,2,4,3],[42],[5,6,7,8]...]
-- Prelude Data.List> intercalate [42] faces
-- [1,2,4,3,42,5,6,7,8...]
-- Prelude Data.List> concat $ intersperse [42] faces
-- [1,2,4,3,42,5,6,7,8...]
-- > :t unlines
-- unlines :: [String] -> String
-- > unlines ["hello ", "world "]
-- "hello \nworld \n"
-- > concatMap (++"\n") ["hello ", "world "]
-- "hello \nworld \n"
-- > map (intersperse " " . (map (show . (*8)))) $ faces
-- [["8"," ","16"," ","32"," ","24"],["40"," ","48"," ","56"," ","64"]...]
-- Prelude Data.List> map (intersperse " " . (map (show . (*8)))) faces
-- [["8"," ","16"," ","32"," ","24"],["40"," ","48"," ","56"," ","64"]]
-- Prelude Data.List> faces
-- [[1,2,4,3],[5,6,7,8]]
-- Prelude Data.List> concatMap (intersperse " " . (map (show . (*8)))) faces
-- ["8"," ","16"," ","32"," ","24","40"," ","48"," ","56"," ","64"]
-- Prelude Data.List> map (intercalate " " . (map (show . (*8)))) faces
-- ["8 16 32 24","40 48 56 64"]
-- Prelude Data.List> map ((++)"f " . intercalate " " . (map (show . (*8)))) faces
-- ["f 8 16 32 24","f 40 48 56 64"]
-- Prelude Data.List> unlines $ map ((++)"f " . intercalate " " . (map (show . (*8)))) faces
-- "f 8 16 32 24\nf 40 48 56 64\n"

-- +-------------------------------------------------------------+
-- | THEN, when trying to prepend the "o <obj-name>" line, BOOM! |
-- +-------------------------------------------------------------+

-- GOTS TO WRAP IT IN ()'s or do something equivalent:
-- > "hello" ++ ( unlines $ map ((++)"f " . unwords . map (show . (+8*3))) faces )
-- "hellof 25 26 28 27\nf 30 29 31 32\nf 25 26 30 29\nf 27 28 32 31\nf 25 27 31 29\nf 26 28 32 30\n"
-- > (++) "hello" $ unlines $ map ((++)"f " . unwords . map (show . (+8*3))) faces
-- "hellof 25 26 28 27\nf 30 29 31 32\nf 25 26 30 29\nf 27 28 32 31\nf 25 27 31 29\nf 26 28 32 30\n"
-- > "hello" ++ ( unlines $ map ((++)"f " . unwords . map (show . (+8*3))) faces )
-- "hellof 25 26 28 27\nf 30 29 31 32\nf 25 26 30 29\nf 27 28 32 31\nf 25 27 31 29\nf 26 28 32 30\n"
-- > f i =  unlines $ map ((++)"f " . unwords . map (show . (+8*i))) faces
-- > "hello" ++ f 3
-- "hellof 25 26 28 27\nf 30 29 31 32\nf 25 26 30 29\nf 27 28 32 31\nf 25 27 31 29\nf 26 28 32 30\n"

-- f [1, 2, 4, 3]
-- f [6, 5, 7, 8]
-- f [1, 2, 6, 5]
-- f [3, 4, 8, 7]
-- f [1, 3, 7, 5]
-- f [2, 4, 8, 6]


-- main = do
--     putStrLn "hello, world!"