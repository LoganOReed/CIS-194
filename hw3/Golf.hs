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

--skips :: [a] -> [[a]]
