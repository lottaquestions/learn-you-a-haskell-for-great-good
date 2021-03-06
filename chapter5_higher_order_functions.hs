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

{--
*Main> zip [1,2,3,4,5] "hello"
zip [1,2,3,4,5] "hello"
[(1,'h'),(2,'e'),(3,'l'),(4,'l'),(5,'o')]
*Main> flip' zip [1,2,3,4,5] "hello"
flip' zip [1,2,3,4,5] "hello"
[('h',1),('e',2),('l',3),('l',4),('o',5)]
*Main> zipWith div [2,2..] [10,8,6,4,2]
zipWith div [2,2..] [10,8,6,4,2]
[0,0,0,0,1]
*Main> zipWith (flip' div) [2,2..] [10,8,6,4,2]
zipWith (flip' div) [2,2..] [10,8,6,4,2]
[5,4,3,2,1]

--}

-- The map function

{--
Prelude> map (+3) [1,5,3,1,6]
map (+3) [1,5,3,1,6]
[4,8,6,4,9]
Prelude> map (++ "!") ["BIFF", "BANG", "POW"]
map (++ "!") ["BIFF", "BANG", "POW"]
["BIFF!","BANG!","POW!"]
Prelude> map (replicate 3) [1 .. 6]
map (replicate 3) [1 .. 6]
[[1,1,1],[2,2,2],[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
Prelude> map (map (^2)) [ [1,2], [3,4,5,6], [7,8] ]
map (map (^2)) [ [1,2], [3,4,5,6], [7,8] ]
[[1,4],[9,16,25,36],[49,64]]
Prelude> map fst [(1,2), (3,5), (6,3), (2,6), (2,5)]
map fst [(1,2), (3,5), (6,3), (2,6), (2,5)]
[1,3,6,2,2]

-- The filter function
Prelude> filter (>3) [1,5,3,2,1,6,4,3,2,1]
filter (>3) [1,5,3,2,1,6,4,3,2,1]
[5,6,4]
Prelude> filter (==3) [1,2,3,4,5]
filter (==3) [1,2,3,4,5]
[3]
Prelude> filter even [1 .. 10]
filter even [1 .. 10]
[2,4,6,8,10]
Prelude> let notNull x = not (null x) in filter notNull [[1,2,3], [], [3,4,5], [2,2], [],[],[]]
let notNull x = not (null x) in filter notNull [[1,2,3], [], [3,4,5], [2,2], [],[],[]]
[[1,2,3],[3,4,5],[2,2]]
Prelude> filter (`elem` ['a' .. 'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
filter (`elem` ['a' .. 'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
"uagameasadifeent"
Prelude> filter (`elem` ['A' .. 'Z']) "i LAuGh at you bEcause u R all the same"
filter (`elem` ['A' .. 'Z']) "i LAuGh at you bEcause u R all the same"
"LAGER"

Prelude> filter (<15) (filter even [1 .. 20])
filter (<15) (filter even [1 .. 20])
[2,4,6,8,10,12,14]
Prelude> [x | x <- [1 .. 20], even x, x < 15]
[x | x <- [1 .. 20], even x, x < 15]
[2,4,6,8,10,12,14]

--}

-- Quicksort using filter
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerOrEqual = filter (<= x) xs
      larger = filter (> x) xs
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger

{--
*Main> quicksort "i LAuGh at you bEcause u R all the same"
quicksort "i LAuGh at you bEcause u R all the same"
"         AEGLRaaaabceeehhillmossttuuuuy"

--}
largestDivisible :: Integer
largestDivisible = head (filter p [99999,99998..])
  where p x = x `mod` 3829 == 0

-- takeWhile
{--
*Main> largestDivisible
largestDivisible
99554
*Main> takeWhile (/=' ') "elephants know how to party"
takeWhile (/=' ') "elephants know how to party"
"elephants"
*Main> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
166650
*Main> sum (takeWhile (<10000)  [m | m <- [n^2 | n <- [1..]], odd m])
sum (takeWhile (<10000)  [m | m <- [n^2 | n <- [1..]], odd m])
166650
--}

-- Collatz chain
chain:: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100]))
  where isLong xs = length xs > 15

{--
*Main> chain 10
chain 10
[10,5,16,8,4,2,1]
*Main> chain 1
chain 1
[1]
*Main> chain 30
chain 30
[30,15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]
*Main> :l chapter5_higher_order_functions.hs
:l chapter5_higher_order_functions.hs
[1 of 1] Compiling Main             ( chapter5_higher_order_functions.hs, interpreted )
Ok, one module loaded.
*Main> numLongChains
numLongChains
66
--}

-- Mapping functions with multiple parameters'
-- let listOfFuns = map (*) [0 ..]
-- (listOfFuns !! 4) 5

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1 .. 100]))

-- Lambdas
-- zipWith (\a b -> (a * 30 + 3) / b) [5, 4, 3, 2, 1] [1, 2, 3, 4, 5]

-- Pattern matching with lambdas
-- map (\(a,b) -> a + b) [(1,2), (3,5), (6,3), (2,6), (2,5)]

-- Similar functions that show that currying is easy/default in Haskell

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

-- When currying notation improves readability

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x

-- zipWith (flip''' (++) ) [" love you ", " love me"] ["I", "You"]
-- map (flip''' subtract 20) [1,2,3,4]

-- Folding
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> x + acc) 0 xs

-- *Main> sum' [3,5,2,1]
-- sum' [3,5,2,1]
-- 11

-- Expressing sum' as using currying
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

{--
*Main> sum'' [3,5,2,1]
sum'' [3,5,2,1]
11
--}

-- Implementing map function using foldr
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc)  [] xs
{--
*Main> map (flip''' subtract 20) [1,2,3,4]
map (flip''' subtract 20) [1,2,3,4]
[19,18,17,16]
*Main> map' (flip''' subtract 20) [1,2,3,4]
map' (flip''' subtract 20) [1,2,3,4]
[19,18,17,16]

--}

-- map function can be implemented using foldl and ++ but ++ is much slower than :
-- so the preference is to use (:)
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs
{--
*Main> map (flip''' subtract 20) [1,2,3,4]
map (flip''' subtract 20) [1,2,3,4]
[19,18,17,16]
*Main> map'' (flip''' subtract 20) [1,2,3,4]
map'' (flip''' subtract 20) [1,2,3,4]
[19,18,17,16]

--}

-- Implementing elem using foldr
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if y == x then True else acc) False ys
{--
*Main> 5 `elem'` [1,2,5,3,4]
5 `elem'` [1,2,5,3,4]
True

--}

-- foldl1 and foldr1 fold without using an explicit initial accumulator, instead
-- they use the first element of the list as the initial accumulator.
-- The max function can be implemented using foldl1

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max
{--
*Main> maximum' [1,10,3,5,6]
maximum' [1,10,3,5,6]
10
--}

-- Implementing reverse with a curried fold function
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
{--
*Main> reverse' [1,2,3,4,5]
reverse' [1,2,3,4,5]
[5,4,3,2,1]
--}

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:) )[]

{--
*Main> reverse'' [1,2,3,4,5]
reverse'' [1,2,3,4,5]
[5,4,3,2,1]

--}

product' :: (Num a) => [a] -> a
product' = foldl (*) 1
{--
*Main> product' [2,3,4]
product' [2,3,4]
24
--}
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

{--
*Main> filter' (\x -> x>3) [1,2,3,4,5]
filter' (\x -> x>3) [1,2,3,4,5]
[4,5]

--}

last' :: [a] -> a
last' = foldl1 (\_ x -> x)
{--
*Main> last' [1,2,3,4,5]
last' [1,2,3,4,5]
5

--}

-- Folding infinite lists
and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs
{--
*Main> and' (repeat False)
and' (repeat False)
False

--}

-- Scans

{--
*Main> scanl (+) 0 [3,5,2,1]
scanl (+) 0 [3,5,2,1]
[0,3,8,10,11]
*Main> scanr (+) 0 [3,5,2,1]
scanr (+) 0 [3,5,2,1]
[11,8,3,1,0]
*Main> scanl1 (+)  [3,5,2,1]
scanl1 (+)  [3,5,2,1]
[3,8,10,11]
*Main> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
[3,4,5,5,7,9,9,9]
*Main> scanl (flip (:)) [] [3,2,1]
scanl (flip (:)) [] [3,2,1]
[[],[3],[2,3],[1,2,3]]

--}

-- How many elements does it take for the sum of the square roots of all natural numbers
-- to exceed 1000?
sqrtSums :: Int
sqrtSums = length ( takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]))) + 1
{--
*Main> sqrtSums
sqrtSums
131
*Main> sum (map sqrt [1..131])
sum (map sqrt [1..131])
1005.0942035344083
*Main> sum (map sqrt [1..130])
sum (map sqrt [1..130])
993.6486803921487

--}


-- Function Application with $
-- How $ is defined
{--
($) :: (a -> b) -> a -> b
f $ x = f x

--}

-- $ is right-associative and is useful for removing parens to make code more readable
{--

This function below,
Prelude> sum (filter (>10) (map (*2) [2 .. 10]))
sum (filter (>10) (map (*2) [2 .. 10]))
80

Can be made more readable using the $
Prelude> sum $ filter (>10) $ map (*2) [2 .. 10]
sum $ filter (>10) $ map (*2) [2 .. 10]
80

--}

-- $ lets us treat function application as any other function, so in the example
-- below, we apply each of the functions in the list to 3

{--
Prelude> map ($ 3) [(4+), (10*), (^2), sqrt]
map ($ 3) [(4+), (10*), (^2), sqrt]
[7.0,30.0,9.0,1.7320508075688772]
--}

-- Function composition
{--
map (negate . abs) [5, -3, -6, 7, -3, 2, -19, 24]

map (negate . sum . tail) [[1..5], [3..6], [1..7]]

Prelude> map (negate . sum . tail) [[1..5], [3..6], [1..7]]
map (negate . sum . tail) [[1..5], [3..6], [1..7]]
[-14,-15,-27]

Prelude> map tail [[1..5], [3..6], [1..7]]
map tail [[1..5], [3..6], [1..7]]
[[2,3,4,5],[4,5,6],[2,3,4,5,6,7]]
Prelude> map sum [[2,3,4,5],[4,5,6],[2,3,4,5,6,7]]

map sum [[2,3,4,5],[4,5,6],[2,3,4,5,6,7]]
[14,15,27]
Prelude>
Prelude> map negate [14,15,27]
map negate [14,15,27]
[-14,-15,-27]

--}

-- Function composition with multiple parameters
{--
-- replacing the below function calls
Prelude> sum (replicate 5 (max 6.7 8.9))
sum (replicate 5 (max 6.7 8.9))
44.5
-- with this one
Prelude> (sum . replicate 5) (max 6.7 8.9)
(sum . replicate 5) (max 6.7 8.9)
44.5
-- or with this one
Prelude> sum . replicate 5 $ max 6.7 8.9
sum . replicate 5 $ max 6.7 8.9
44.5

-- Replacing the below chain of calls
Prelude> replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
[180,180]
Prelude> zipWith max [1,2] [4,5]
zipWith max [1,2] [4,5]
[4,5]
Prelude> map (*3) $ zipWith max [1,2] [4,5]
map (*3) $ zipWith max [1,2] [4,5]
[12,15]
Prelude> product . map (*3) $ zipWith max [1,2] [4,5]
product . map (*3) $ zipWith max [1,2] [4,5]
180

-- With these composed calls
Prelude> replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]
replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]
[180,180]

--}

-- Point-free style
-- Rewriting sum' in point-free style
sum''' :: (Num a) => [a] -> a
sum''' = foldl (+) 0


fn x = ceiling (negate (tan (cos (max 50 x))))
fn' = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<1000) (filter odd (map (^2) [1..])))

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<1000) . filter odd $ map (^2) [1..]

{--
-- Test cases for the above functions
*Main> sum''' [10, 10, 10]
sum''' [10, 10, 10]
30
*Main> fn 70
fn 70
0
*Main> fn 40
fn 40
-1
*Main> oddSquareSum
oddSquareSum
5456
*Main> oddSquareSum'
oddSquareSum'
5456

--}
