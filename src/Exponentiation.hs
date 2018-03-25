{-# LANGUAGE OverloadedStrings #-}
module Exponentiation where

-- modular exponentiation

{--
function modular_pow(base, exponent, modulus)
    if modulus = 1 then return 0
    result := 1
    base := base mod modulus
    while exponent > 0
        if (exponent mod 2 == 1):
           result := (result * base) mod modulus
        exponent := exponent >> 1
        base := (base * base) mod modulus
    return result
-}

modExp :: Integer -> Integer -> Integer -> Integer -> Integer
modExp b 0 m res = res
modExp b e m res
  | e `mod` 2 == 1 = modExp ((b * b) `mod` m) (e `div` 2) m (res * b `mod` m)
  | otherwise = modExp ((b * b) `mod` m) (e `div` 2) m res

fermatPT :: Integer -> Bool
fermatPT n = all (fermatTest n) (filter (\a -> gcd n a == 1) [1..n-1])
             where fermatTest n a = modExp a (n-1) n 1 == 1

findds :: Integer -> (Integer, Integer)
findds n = f 0 n
  where
    f d m
      | r == 1 = (d, m)
      | otherwise = f (d+1) s
       where (s, r) = quotRem m 2

squareMod :: Integer -> Integer -> Integer
squareMod n a = (a * a) `rem` n

millerRabinPrimalityTest :: Integer -> Integer -> Bool
millerRabinPrimalityTest n a
  | a <= 1 || a >= n - 1 = error $ "witness a out of range "
  | n < 2 = False
  | even n = False
  | b0 == 1 || b0 == n' = True
  | otherwise = iter (tail b)
  where
    n' = n - 1
    (d, s) = findds n'
    b0 = modExp a s n 1 -- a^s mod n 
    b = take (fromIntegral d) $ iterate (squareMod n) b0
    iter [] = False
    iter (x:xs)
      | x == 1 = False
      | x == n' = True
      | otherwise = iter xs
