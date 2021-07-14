-- creditCard.hs --

-- Goal: Validate a given credit card number using the following operations:
--  1. Starting from the second to last digit and going backwards, double every other number
--  2. Add every individual digit together (so if there is e.g a 15 in the list, we add 1 and 5 seperately)
--  3. Calculate the number from step 2 mod 10

-- converts pos int to list of digits
toDigits :: Integer -> [Integer]

toDigits num 
    | num < 0   = [0]
    | num <= 10 = [num]
    | otherwise = toDigits (num `div` 10) ++ [num `mod` 10]
    

-- same as toDigits, but reverse
toDigitsRev :: Integer -> [Integer]

toDigitsRev num 
    | num < 0   = [0]
    | num <= 10  = [num]
    | otherwise = num `mod` 10 : toDigitsRev (num `div` 10)

doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther xs
    | length ns >= 2 = let (a:b:c) = ns in doubleEveryOther (reverse c) ++ b+b:[a]
    | otherwise      = xs
    where ns = reverse xs