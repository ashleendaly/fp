module Main where

max2:: Int -> Int -> Int
max2 x y = if x > y then x else y

max3:: Int -> Int -> Int -> Int
-- max3 x y z = max2 max2 x y z
max3 x y z = max2 x (max2 y z)

f :: (Int-> String)-> (String-> Bool)-> (Int-> Bool)
f func1 func2 = func2 . func1

g :: (Int-> Bool)-> (Bool-> String)-> Int-> String
g func1 func2 = func2 . func1

twice :: (Int-> Int)-> Int-> Int
twice func x = func (func x)

physics:: Float -> Float -> Float -> Float
physics m1 m2 d = do
    let g = 6.67 * 10^^(-11)
    (g*m1*m2)/d

main :: IO ()
main = do
    print (max2 10 20)
    print(max3 5 10 20)
    print(physics 1 1 1)