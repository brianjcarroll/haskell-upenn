{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits x
  | x <= 0    = []
  | otherwise = (lastDigit x) : (toRevDigits $ dropLastDigit x)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : (y * 2) : (doubleEveryOther xs)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = sum [ if x >= 10 then x - 9 else x | x <- xs]


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = (lastDigit $ sumDigits $ doubleEveryOther $ toRevDigits x) == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 source target _ = [(source, target)]
hanoi n source target aux = hanoi (n - 1) source aux target ++ [(source, target)] ++ hanoi (n - 1) aux target source
