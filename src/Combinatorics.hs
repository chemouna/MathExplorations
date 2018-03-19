
module Combinatorics where

choose n k = product [n, n-1 .. n - k + 1] `div` product [1..k]
  
