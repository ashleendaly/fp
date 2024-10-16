module Main where

isbnCheck :: [Int] -> Int
isbnCheck isbn = 11 - (sum [x*y | (x, y) <- zip isbn (reverse [2..10])] `mod` 11)

main :: IO ()
main = do
    print (isbnCheck [0,2,6,2,1,6,2,0,9])
    print (isbnCheck [9,9,9,2,1,5,8,1,0])
    print (isbnCheck [8,0,9,0,2,7,3,4,1])