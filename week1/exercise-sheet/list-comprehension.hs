module Main where

triangles :: Int-> [Int]
triangles n = [(x * (x+1)) `div` 2 | x <- [1..n]] 

primes :: Int-> [Int]
primes n = [x | x <- [1..n], n `mod` x == 0]

flatten :: [[a]]-> a
flatten to_flatten = []

main :: IO ()
main = do
    print [x | x <- [1..30], x `mod` 3 == 0]
    print (triangles 5) 
    print (primes 5)
    print (primes 20)
    print (flatten [[1,2,3], [4,5,6], [7,8,9]])