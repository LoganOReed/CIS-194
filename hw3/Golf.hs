{-# OPTIONS_GHC -Wall #-}

module Golf where

---------------
-- ASSIGNMENT SUMMARY:
--  Goal is to practice using standard library functions
--  Extra stress on making compact functions
---------------


---------------
-- EXERCISE ONE:
--  The nth list in the output should 
--      contain every nth item in the input.
--  e.g skips "abcd" = ["abcd", "bd","c",d]
---------------

-- creates a list of ordered pairs that 
-- have the index of the element and the element itself
indexList :: [a] -> [(Int,a)]
indexList xs = zip [1 .. ] xs

indexSelect :: Int -> [(Int,a)] -> [(Int,a)]
{-
indexSelect n xs = 
    [snd (x,y) | (x,y) <- xs, x `mod` n == 0]
-}


indexSelect n xs =
    filter ((\x -> (x `mod` n) == 0).fst) xs


fullIndexSelect :: [(Int,a)] -> Int -> [a]
fullIndexSelect xs n =
    snd . unzip $ filter ((== 0) . (flip mod n) . fst) xs

-- use list comprehension and fullIndexSelect to construct the multi list
skips :: [a] -> [[a]]

--skips xs =
--   [fullIndexSelect (indexList xs) n | n <- [1 .. length xs]]

--skips xs =
--  map (fullIndexSelect (zip [1 .. ] xs) ) [1 .. length xs]

-- the top iteration makes most sense, map is just syntactic sugar for set builder
skips xs =
    map (fullIndexSelect (zip [1 .. ] xs) ) [1 .. length xs]



--------------------------
-- ASSIGNMENT TWO: LOCAL MAXIMA
--  a function that takes a list and returns all 
--      local maxima in order
---------------------------

-- pattern matches all triples and appends them if it's a local max
localMaxima :: [Integer] -> [Integer]

--localMaxima (a:b:c:xs) = 
--    [a:b:c:[]] ++ localMaxima (b:c:xs)

localMaxima (a:b:c:xs) = 
    if a < b && c < b
        then b:localMaxima (b:c:xs)
        else localMaxima (b:c:xs)

localMaxima _ =
    []



---------------------------
-- PROBLEM THREE: HISTOGRAM
-- a function takes in a list of single digits and displays 
--      their histogram graphically
----------------------------

starsSpace :: Int -> Int -> Char

starsSpace cut n
    | n > cut   = '*'
    | otherwise = ' '

histogram :: [Int] -> String

histogram xs =
    unlines $ reverse [ [starsSpace cut x | x <- xs] | cut <- [0 .. maximum xs] ]