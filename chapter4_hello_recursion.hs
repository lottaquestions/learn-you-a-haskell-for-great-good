maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
-- Test
-- maximum' [1,2,3,4]

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x: replicate' (n - 1) x
-- Test
-- replicate' 3 5

take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n -1) xs
-- Test
-- take' 4 [1 .. 30]

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
-- reverse' [1,2,3,4]

repeat' :: a -> [a]
repeat' x = x:repeat' x
-- take 5 (repeat' 3)

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- zip' [1,2,3] ['a','b','c']
-- [(1,'a'),(2,'b'),(3,'c')]

-- zip' [1,2,3] ['a','b']
-- [(1,'a'),(2,'b')]

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = a `elem'` xs
{--
5 `elem'` [1,2,3,4,5]
True
*Main> 5 `elem'` [1,2,3,4]
5 `elem'` [1,2,3,4]
False
*Main> 5 `elem'` []
5 `elem'` []
False
--}
