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

{--
main :: IO ()
main =
  print $
  modExp
    2988348162058574136915891421498819466320163312926952423791023078876139
    2351399303373464486466122544523690094744975233415544072992656881240319
    (10 ^ 40)
    1
-}
