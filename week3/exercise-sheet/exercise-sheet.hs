import System.Random
import Data.Ord

{- Parametric Polymorphism -}

-- Q1
rotate :: (a, b, c) -> (c, a, b)
rotate (x,y,z) = (z,x,y)

-- Q2
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x, y)

-- Q3
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x, y)= f x y

-- Q4
myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f y x = f x y

-- Q5
f :: (a, b) -> (a -> c) -> (b -> d) -> ((c, d) -> e) -> e
f (x, y) f1 f2 transform_tuple = transform_tuple (f1 x, f2 y)

{- Typeclasses -}

-- Q1: Define a Vehicle ADT
data Vehicle = Van | Car | Lorry 
--
-- Q2, Q3: How do we modify to support printing the name and deciding equality?
    deriving (Show, Eq)

-- Q4
-- class Wheely v where
--     numWheels :: v -> Int

-- instance Wheely Vehicle where
--     numWheels Van = 4
--     numWheels Car = 4
--     numWheels Lorry 12

numWheels :: Vehicle -> Int
numWheels Car = 4
numWheels Van = 4
numWheels Lorry = 12

-- Q5: How do we implement an ordering on wheels?
instance Ord Vehicle where
    compare Car Lorry = LT
    compare Car Van = EQ
    compare Lorry Car = GT


{- Lazy evaluation -}
ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

-- (1)
butIfZero :: Integer -> Integer -> Integer
butIfZero x y = if x /= 0 then x else y

-- (2)

-- let f = \x y z -> if (x=="hello" && y=="haskell") then z else 42

-- (a)

-- What is the type of f?
-- f :: String -> String -> p -> p | Int

-- Result from :t
--  f :: Num p => String -> String -> p -> p

-- (b)
-- How would you describe f in terms of evaluation of its perameters?
-- Which must always be evaluated?
-- If x and y are not "hello" and "haskell", then z is not evaluated.
-- x and y must always be evaluated

-- (c)
-- Would you expect the following function evaluation to take a long time? Why/why not?
-- f "hello" "c++" (ackermann 4 2)
-- No I would not expect this to take a long time because the conditional returns false so z is not evaluated and 42 will be rapidly returned

-- (d) Given the strictness annotation on z, would you expect evaluating
-- f "goodbye" "java" (ackermann 4 2) would take a long time?
-- Yes I would expect this to take a long time because z must be evaluated and (ackermann 4 2) will take long to evaluate

-- (e) Can you see any useful reason for strictness annotations in Haskell?
-- Yes if you want to make sure something evaluates because you want to maybe see if it will throw an error or test whether there is a bug during evaluation

{- Further exercises -}
gen = mkStdGen 137

randomElem :: (RandomGen g) => g -> [a] -> (a, g)
randomElem gen xs = uniformR (min xs, max xs) gen

randomElements :: Int -> [a] -> [a]
randomElements _ = undefined