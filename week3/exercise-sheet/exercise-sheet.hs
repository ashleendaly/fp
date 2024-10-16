import System.Random
import Data.Ord

{- Parametric Polymorphism -}

-- Q1
rotate :: (a, b, c) -> (c, a, b)
rotate = undefined

-- Q2
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry = undefined

-- Q3
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry = undefined

-- Q4
myFlip :: (a -> b -> c) -> b -> a -> c
myFlip = undefined

-- Q5
f :: (a, b) -> (a -> c) -> (b -> d) -> ((c, d) -> e) -> e
f = undefined

{- Typeclasses -}

-- Q1: Define a Vehicle ADT
--
-- Q2, Q3: How do we modify to support printing the name and deciding equality?

-- Q4
-- numWheels :: Vehicle -> Int

-- Q5: How do we implement an ordering on wheels?

{- Lazy evaluation -}
ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

-- (1)
butIfZero :: Integer -> Integer -> Integer
butIfZero = undefined

-- (2)
{-
let f = \x y z -> if (x=="hello" && y=="haskell") then z
                  else 42
-}
-- (a)

-- What is the type of f?

-- (b)
-- How would you describe f in terms of evaluation of its perameters?
-- Which must always be evaluated?
--

-- (c)
-- Would you expect the following function evaluation to take a long time? Why/why not?
-- f "hello" "c++" (ackermann 4 2)

-- (d) Given the strictness annotation on z, would you expect evaluating
-- f "goodbye" "java" (ackermann 4 2) would take a long time?

-- (e) Can you see any useful reason for strictness annotations in Haskell?

{- Further exercises -}

randomElem :: (RandomGen g) => g -> [a] -> (a, g)
randomElem _ = undefined

randomElements :: Int -> [a] -> [a]
randomElements _ = undefined