-- Below is same as bash cmds in c --
{-# OPTIONS_GHC -Wall #-}

-- Solution to the classic Towers Of Hanoi Puzzle --

-- STEPS
-- move n−1 discs from a to c using b as temporary storage
-- move the top disc from a to b
-- move n−1 discs from c to b using a as temporary storage.

type Peg = String
type Move = (Peg, Peg)

-- takes in the number of discs and the names of the 3 pegs and returns a list of moves
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi 1 a _ c = [(a,c)]
hanoi n a b c = 
    (hanoi $ n - 1) a c b ++ (a,c):(hanoi $ n - 1) b a c

