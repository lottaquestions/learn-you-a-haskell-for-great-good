-- :t 'a'
-- :t True
-- :t "Hello"
-- :t (True, 'a')
-- :t 4==5

removeUpperCase :: [Char] -> [Char]
removeUpperCase st = [c | c<-st, c`elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x+y+z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- Type classes
-- :t head
-- :t fst
-- :t (==)
-- :t (>)
-- "Abrakadabra" < "Zebra"
-- "Abrakadabra" `compare` "Zebra"
-- 5 `compare` 3

-- Show type class
-- show 3
-- show 5.334
-- show True

-- Read type class
-- read "True" || False
-- read "8.2" + 3.8
-- read "5" - 2
-- read "[1,2,3,4]" ++ [3]
-- :t read

-- Type annotations
-- read "5" :: Int
-- read "5" :: Float
-- (read "5" :: Float) * 4
-- (read "[1,2,3,4]" :: [Int])
-- (read "(3,'a')") :: (Int,Char)
-- [read "True", False, True, False]

-- Enum types
-- ['a'..'e']
-- [LT .. GT]
-- [3..5]
-- succ 'B'

-- Bounded type class
-- minBound :: Int
-- maxBound :: Char
-- minBound :: Bool
-- maxBound :: Bool
-- maxBound :: (Bool, Int, Char)

-- Num type class
-- :t 20
-- 20 :: Int
-- 20 :: Integer
-- 20 :: Float
-- 20 :: Double
-- :t (*)

-- Integral type class
-- Below is the fromIntegral type class type declaration, it already exists, do not rerun it
-- fromIntegral :: (Integral a, Num b) => a -> b
-- :t length
-- fromIntegral (length [1,2,3,4]) + 3.2

data Dir = N | E | S | W deriving (Show, Eq, Bounded, Ord)
instance Enum Dir where
  toEnum i = convert (i `mod` fromEnum (maxBound :: Dir))
    where
      convert 0 = N
      convert 1 = E
      convert 2 = S
      convert 3 = W
  fromEnum N = 0
  fromEnum E = 1
  fromEnum S = 2
  fromEnum W = 3

combineDirection :: Dir -> Dir -> Dir
combineDirection a b = min a b
instance Semigroup Dir where
  (<>) = comb'3
   where
      comb'3 :: Dir -> Dir -> Dir
      comb'3 a b = toEnum (a' + b')
        where
          a' = fromEnum a
          b' = fromEnum b

instance Monoid Dir where
  mempty = N



dirs :: [] Dir
dirs = [N .. W]

nums :: [] Int
nums = fmap fromEnum dirs

dirs'1 :: [Dir]
dirs'1 = fmap toEnum [1 .. 10]

type Pos = (Int, Int)
-- Given a starting position, in a grid and a list of directions, find out
-- what the final position in the grid is.

type Dirs = [Dir]
trip :: Dirs -> Pos -> Pos
trip [] pos = pos
trip (dir:dirs) (x,y)
  | dir == N = trip dirs (x,y+1)
  | dir == S = trip dirs (x,y-1)
  | dir == W = trip dirs (x-1,y)
  | dir == E = trip dirs (x+1,y)

trip'1 :: Dirs -> Pos -> Pos
trip'1 dirs pos = foldl step pos dirs
  where
    step (x,y) N = (x,y+1)
    step (x,y) S = (x,y-1)
    step (x,y) W = (x-1,y)
    step (x,y) E = (x+1,y)

trip'2 :: Dirs -> Pos -> [Pos]
trip'2 dirs pos = scanl step pos dirs
  where
    step (x,y) N = (x,y+1)
    step (x,y) S = (x,y-1)
    step (x,y) W = (x-1,y)
    step (x,y) E = (x+1,y)

-- A function that has constraints of how far in the (x,y) grid we can
-- go. In other words, it has the boundaries of the grid we are in

trip'3 :: (Pos -> Pos) ->  Dirs -> Pos -> [Pos]
trip'3  pred dirs pos = scanl step pos dirs
  where
    step (x,y) N = pred (x,y+1)
    step (x,y) S = pred (x,y-1)
    step (x,y) W = pred (x-1,y)
    step (x,y) E = pred (x+1,y)

-- Expressing a computation that might fail
trip'4 :: (Pos -> Maybe Pos) -> (Pos -> Pos) ->  Dirs -> Pos -> Maybe [Pos]
trip'4  pred clamper dirs pos = case pred pos of
  Just pos' -> Just  (trip'3  clamper dirs pos')
  Nothing -> Nothing
-- case (min 4 (max 0 x) == x, min 4 (max 0 y) == y) of (True, True) -> Just (x,y); (_,_) -> Nothing

-- case (min 4 (max 0 x) == x, min 4 (max 0 y) == y) of (True, True) -> Just (x,y); (  ,  ) -> Nothing
