-- Curried functions
multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

{--
*Main> let multTwoWithNine = multThree 9
let multTwoWithNine = multThree 9
*Main> multTwoWithNine 2 3
multTwoWithNine 2 3
54
--}

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x
{--
*Main> compareWithHundred 99
compareWithHundred 99
GT
--}

-- Sections
divideByTen :: (Floating a) => a -> a
divideByTen  = (/10)

{--
*Main> divideByTen 200
divideByTen 200
20.0
--}

isUpperAlphaNum :: Char -> Bool
isUpperAlphaNum = ( `elem` ['A' .. 'Z'] )

{--
*Main> isUpperAlphaNum 'a'
isUpperAlphaNum 'a'
False
*Main> isUpperAlphaNum 'P'
isUpperAlphaNum 'P'
True
*Main> isUpperAlphaNum 'Q'
isUpperAlphaNum 'Q'
True
--}

applyTwice :: (a ->a ) -> a -> a
applyTwice f x = f (f x)

{--
*Main> applyTwice (+3) 10
applyTwice (+3) 10
16
*Main> applyTwice (++ "HAHA") "HEY"
applyTwice (++ "HAHA") "HEY"
"HEYHAHAHAHA"
*Main> applyTwice (++ " HAHA ") "HEY"
applyTwice (++ " HAHA ") "HEY"
"HEY HAHA  HAHA "
*Main> applyTwice (multThree 2 2) 9
applyTwice (multThree 2 2) 9
144
*Main> 4 *(4*9)
4 *(4*9)
144
*Main> applyTwice (3:) [1]
applyTwice (3:) [1]
[3,3,1]
--}


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

{--
*Main> zipWith' (+) [4,2,5,6] [2,6,2,3]
zipWith' (+) [4,2,5,6] [2,6,2,3]
[6,8,7,9]
*Main> zipWith' max [6,3,2,1] [7,3,1,5]
zipWith' max [6,3,2,1] [7,3,1,5]
[7,3,2,5]
*Main> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
["foo fighters","bar hoppers","baz aldrin"]
*Main> replicate 5 2
replicate 5 2
[2,2,2,2,2]
*Main> zipWith' (*) (replicate 5 2) [1..]
zipWith' (*) (replicate 5 2) [1..]
[2,4,6,8,10]
*Main> zipWith' (zipWith' (*)) [[1,2,3], [3,5,6], [2,3,4]] [ [3,2,2], [3,4,5],[5,4,3]]
zipWith' (zipWith' (*)) [[1,2,3], [3,5,6], [2,3,4]] [ [3,2,2], [3,4,5],[5,4,3]]
[[3,4,6],[9,20,30],[10,12,12]]
--}

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where
    g x y = f y x

-- Note that the arrow -> is right-associative, so (a -> b -> c) -> (b -> a -> c) is the
-- same as (a -> b -> c) -> b -> a -> c
-- A simpler version of the above function that takes advantage of currying is:

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y
