-- Copyright © 2012 Edward O'Callaghan. All Rights Reserved.
--

-- Problem Source: Project Eular - Problem 1.
-- ******************************************
-- If we list all the natural numbers below 10 that are multiples of 3 or 5,
-- we get 3, 5, 6 and 9. The sum of these multiples is 23.
--
-- Find the sum of all the multiples of 3 or 5 below 1000.

module Main where

main :: IO ()
main = print (sumup 1000)
-- answer: 233168

sumup :: Int -> Int
sumup n = sum [x | x <- [1..(n-1)], hasThree x || hasFive x]

hasFive :: Int -> Bool
hasFive x | x`rem`5 == 0 = True
          | otherwise = False

hasThree :: Int -> Bool
hasThree x | x`rem`3 == 0 = True
           | otherwise = False
