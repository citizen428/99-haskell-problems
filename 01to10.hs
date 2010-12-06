-- Problem 1

myLast :: [a] -> a
myLast []     = error "empty list"
myLast [x]    = x
myLast (_:xs) = myLast xs 

myLast' = head . reverse

-- Problem 2

myButLast :: [a] -> a
myButLast = last . init

myButLast' = head . drop 1 . reverse -- like myLast'

myButLast'' [x,_]  = x
myButLast'' (_:xs) = myButLast'' xs

-- Problem 3

elementAt :: [a] -> Int -> a
elementAt xs n = xs !! (n - 1)

-- Problem 4

myLength :: [a] -> Int
myLength xs = sum [ 1 | _ <- xs ]

myLength' xs = snd $ last $ zip xs [1..]

-- Problem 5

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x] -- naive

myReverse' =  foldl (flip (:)) [] -- Prelude version

-- Problem 6

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Problem 7

-- Haskell doesn't support arbitrarily nested lists,
-- so flatten would basically be the same as 'concat'.
-- We can however define or own list type:

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- Problem 8

compress :: Eq a => [a] -> [a]
compress []     = []
compress (x:xs) = [x] ++ (compress $ dropWhile (== x) xs)

-- Problem 9

pack :: Eq a => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

-- Problem 10

-- using Problem 9 as requested
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs)

-- quick and very dirty
encode' []     = []
encode' (x:xs) = ((+1) $ length $ takeWhile (==x) xs, x) : 
                (encode $ dropWhile (==x) xs)
