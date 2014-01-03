-- Copyright © 2014 Edward O'Callaghan. All Rights Reserved.
--

-- Problem Source: Project Eular - Problem 3.
-- ******************************************

-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?

module Main where

import Data.Natural

--
-- Given Prime to factor
givenPrime :: Natural
givenPrime = 600851475143

--
-- Find primes using Sieve of Eratosthenes
primes :: [Natural]
primes = sieve [2..]
  where
    sieve (p:ns) = p : sieve [n | n <- ns, n `mod` p > 0]

--
-- Generate a list of prime factors
factors :: Natural -> [Natural]
factors x = factor x primes
  where
    factor n (p:ps)
        | p*p > n       = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise     = factor n ps

--
-- Find largest prime element
largestPrime :: Natural -> Natural
largestPrime x = maximum (factors x)

--
-- Calculate answer..
main :: IO ()
main = print (largestPrime givenPrime)
-- answer: 6857
