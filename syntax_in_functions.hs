lucky :: Int -> String
lucky 7 = "LUCKY NUMBER 7!"
lucky x = "Sorry, you are out of luck pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"


factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Pattern matching in tuples
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_, x, _) = x

third  :: (a,b,c) -> c
third (_, _, x) = x

-- Pattern matching with lists and list comprehensions
xs = [(1,3), (4,3),(2,4),(5,3),(5,6),(3,1)]
result1 = [a+b | (a,b) <-xs]

head' :: [a] -> a
head' []    = error "cannot call head' on the empty list"
head' (x:_) = x
-- head' [3,4,5]
-- head' "Hello"

tell :: Show (a) => [a] -> String
tell []       = "The list is empty"
tell (x:[])   = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_)  = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- tell []
-- tell [1]
-- tell [1,2]
-- tell [1,2,3]

-- As-patterns
firstLetter :: String -> String
firstLetter "" = "The string is empty!"
firstLetter all@(x:y:z) = "The first letter of " ++ all ++ " is " ++ show x
-- firstLetter "The count is here"

-- Guards
bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You are underweight. Eat more!"
    | bmi <= 25.0 = "Looking good!"
    | bmi <= 30.0 = "You're overweight. Let's work out together"
    | otherwise = "You are obese. Go see a doctor"


bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | weight / height ^ 2 <= 18.5 = "You are underweight. Eat more!"
    | weight / height ^ 2 <= 25.0 = "Looking good!"
    | weight / height ^ 2 <= 30.0 = "You're overweight. Let's work out together"
    | otherwise = "You are obese. Go see a doctor"

-- bmiTell' 85 1.9

bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
    | bmi <= 18.5 = "You are underweight. Eat more!"
    | bmi <= 25.0 = "Looking good!"
    | bmi <= 30.0 = "You're overweight. Let's work out together"
    | otherwise = "You are obese. Go see a doctor"
    where bmi = weight / height ^ 2

bmiTell''' :: Double -> Double -> String
bmiTell''' weight height
    | bmi <= skinny = "You are underweight. Eat more!"
    | bmi <= normal = "Looking good!"
    | bmi <= overweight = "You're overweight. Let's work out together"
    | otherwise = "You are obese. Go see a doctor"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          overweight = 30.0

bmiTell'''' :: Double -> Double -> String
bmiTell'''' weight height
    | bmi <= skinny = "You are underweight. Eat more!"
    | bmi <= normal = "Looking good!"
    | bmi <= overweight = "You're overweight. Let's work out together"
    | otherwise = "You are obese. Go see a doctor"
    where bmi = weight / height ^ 2
          (skinny,normal, overweight) = (18.5,25.0,30.0)

badGreeting :: String
badGreeting = "Oh! Pfft! It's you!"

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you, "

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name

-- A function that obtains initials when given first name and last name
initials :: String -> String ->  String
initials firstname lastname  = [f] ++ ". " ++ [l]  ++ "."
    where
      (f:_) = firstname
      (l:_) = lastname

-- A function that takes a list of weight/height pairs and returns a BMI
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w,h) <-xs ]
    where
      bmi weight height = weight / height ^ 2
