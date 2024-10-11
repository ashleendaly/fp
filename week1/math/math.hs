module Main where

addOne:: Int -> Int
addOne x = x + 1

main :: IO ()
main = do
    print (map addOne [1, 2, 3])
    print (addOne 1)