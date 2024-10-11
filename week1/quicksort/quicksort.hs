module Main where

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    quicksort [y | y <- xs, y <= x] ++
    [x] ++
    quicksort [y | y <- xs, y > x]

main :: IO ()
main = do
    let to_sort = [4,5,2,10]
    print to_sort
    let result = quicksort to_sort
    print result