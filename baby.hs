doubleMe  x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                      then x
                      else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's me, Conan O'Brien"

lostNumbers = [4, 8, 15, 16, 23, 42]
-- [1,2,3,4] ++ [9,10,11,12]
-- "hello" ++ " " ++ "world"
-- ['w' , 'o'] ++ ['o', 't']
-- 'A' : " SMALL CAT"
-- 5:[1,2,3,4,5]
-- [1,2,3,4] ++ [5]
-- 1:2:3:4:[]
-- "Steve Buscemi" !! 6
-- [9.4, 33.2,96.2,11.2,23.25] !! 1
-- let b = [[1,2,3,4], [5,3,3,3],[1,2,3,4], [1,2,3]]
-- b ++ [[1,1,1,1]]
-- [7,7,7]:b
-- b !! 2
-- [3,2,1] > [2,1,0]
-- [3,2,1] > [2,10,100]
-- [3,4,2] < [3,4,3]
-- [3,4,2] < [3,4,3]
-- [3,4,2] > [2,4]
-- [3,4,2] ==  [3,4,2]
-- head [5,4,3,2,1]
-- tail [5,4,3,2,1]
-- last [5,4,3,2,1]
-- last (tail [5,4,3,2,1])
-- init [5,4,3,2,1]
-- length [5,4,3,2,1]
-- null [5,4,3,2,1]
-- null []
-- reverse [5,4,3,2,1]
-- take 3 [5,4,3,2,1]
-- take 1 [5,4,3,2,1]
-- take 0 [5,4,3,2,1]
-- drop 3 [8,4,2,1,5,6]
-- drop 0 [8,4,2,1,5,6]
-- drop 100 [8,4,2,1,5,6]
-- maximum [1,9,2,3,4]
-- minimum [8,4,2,1,9,6]
-- sum [5,2,1,6,3,2,5,7]
-- product [6,2,1,2]
-- product [6,2,1,2,0]
-- 4 `elem` [3,4,5,6]
-- 10 `elem` [3,4,5,6]
-- [1..20]
-- ['a'..'z']
-- ['K'..'Z']
-- [2,4..20]
-- [3,6..20]
-- [20,19..1]
-- take 10 (cycle [1,2,3])
-- take 10 (repeat 5)
-- replicate 3 10
-- List comprehension
-- [x*2 | x <- [1..10]]
-- List comprehension with a predicate/condition
-- Note: Predicates go at the end of the list comprehension, and are separated
--       from the rest of the comprehension by a comma
-- [x*2 | x <- [1..10], x*2>12]
-- List comprehension of all numbers from 50 to 100 whose remainder when divided by 7 is 3
-- [x | x <- [50,51..100], x `mod` 7 == 3]

boomBangs xs = [if x < 10 then "BOOM" else "BANG" | x <- xs, odd x]

-- [x | x <- [10..20], x/=13, x/=15, x/=19]
-- [x+y | x<-[1,2,3], y<-[10,100,1000]]
-- [x*y | x <- [2,5,10], y<-[8,10,11]]
-- [x*y | x <- [2,5,10], y<-[8,10,11], x*y >50]
-- let nouns = ["hobbo", "frog", "pope"]
-- let adjectives = ["lazy", "grouchy", "scheming"]
-- [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
-- Custom length function that takes a list as an input, replaces each element with
-- 1 and gets the sum of the resulting list
length' xs = sum [1 | _ <-xs]

removeNoneUpperCase st = [c | c<-st, c `elem` ['A'..'Z']]

-- let xxs=[[1,3,5,2,3,1,2,4,5], [1,2,3,4,5,6,7,8,9], [1,2,4,2,1,6,3,1,3,2,3,6]]
-- [[x | x<-xs, even x] | xs <- xxs]
-- Tuples
-- fst (8,11)
-- snd (8,11)
-- zip [1,2,3,4,5] [5,5,5,5,5]
-- zip [1..5] ["one", "two", "three", "four", "five"]
-- zip [5,3,2,6,2,7,2,5,4,6,6] ["in", "a", "turtle"]
-- zip [1..] ["apple", "orange", "cherry", "mango"]
-- Steps to create a right-angle triangle in which a^2+b^2=c^2,
-- and a<=10 and b<=10 and c <=10 and the perimeter of the triangle is 24
-- [(a,b,c) | a<-[1..10], b <-[1..10], c<-[1..10]]
-- let rightTriangles = [(a,b,c) |  c<-[1..10], a<-[1..c], b <-[1..a], (a^2 + b^2 == c^2)]
-- let rightTriangles = [(a,b,c) |  c<-[1..10], a<-[1..c], b <-[1..a], (a^2 + b^2 == c^2), a+b+c==24]
-- :t 'a'
-- :t True
-- :t "Hello"
-- :t (True, 'a')
-- :t 4==5
-- test
