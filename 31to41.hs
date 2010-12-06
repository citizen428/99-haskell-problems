-- Problem 31

isPrime :: Integral a => a -> Bool
isPrime n = length (factors n 2) == 1 where
    factors 1 _ = []
    factors n factor
      | n `mod` factor == 0 = factor : factors (n `div` factor) factor
      | otherwise           = factors n (factor + 1)
