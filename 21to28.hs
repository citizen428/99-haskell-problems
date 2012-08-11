-- Problem 21

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs (n+1) = start ++ x : end 
  where
    (start, end) = splitAt n xs

-- Problem 22

range :: Int -> Int -> [Int]
range n m | n < m     = take (m-n+1) $ iterate (+1) n 
          | n == m    = [n]
          | otherwise = take (n-m+1) $ iterate (+(-1)) n

-- Problem 23

-- Problem 24

-- Problem 25

-- Problem 26

-- Problem 27

-- Problem 28
