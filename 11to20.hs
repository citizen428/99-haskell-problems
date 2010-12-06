-- Problem 11

data EncodingList a = Single a | Multiple Int a deriving (Show)

encodeModified :: Eq a => [a] -> [EncodingList a]
encodeModified = map encodeHelper . encode 
  where
    encodeHelper (1,x) = Single x 
    encodeHelper (n,x) = Multiple n x

-- Problen 12
    
decodeModified :: [EncodingList a] -> [a]
decodeModified = concatMap decodeHelper 
  where
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple n x) = replicate n x

-- Problem 13
    
-- missing

-- Problem 14

dupli :: [a] -> [a]
dupli = concatMap (replicate 2) 

-- Problem 15

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- Problem 16

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map fst $ filter ((n/=) . snd) $ zip xs [1..]

-- Problem 17

-- missing

-- Problem 18

slice :: [a] -> Int -> Int -> [a]
slice xs n n' = take (n'-n+1) $ drop (n-1) xs 

-- Problem 19

rotate :: [a] -> Int -> [a]
rotate xs n | n >= 0 = drop n xs ++ take n xs
            | n < 0 = drop len xs ++ take len xs
                      where len = n+length xs
                            
rotate' xs n = drop n' xs ++ take n' xs 
  where n' = n `mod` length xs

-- Problem 20

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs!!(n-1) , take (n-1) xs ++ drop n xs) 
