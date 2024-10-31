import Data.Char
import Control.Monad
{- Week 4 exercises -}

-- IO
-- Q1
echoCaps :: IO ()
echoCaps = do
    str <- getLine
    let upperStr = map toUpper str
    putStrLn upperStr

-- Q2
echoFile :: FilePath -> IO ()
echoFile path = do
    file <- readFile path
    let file_lines = lines file
    printLines file_lines
    where
        printLines [] = return ()
        printLines (x:xs) = do
            putStrLn x
            printLines xs


-- Q3
calculator :: IO ()
calculator = do
    operation <- getLine
    let valid_operation = if operation /= "+" && operation /= "*" && operation /= "-"
        then "+"
        else operation
    num1 <- getLine
    num2 <- getLine

    let real_num1 = read num1 :: Int
    let real_num2 = read num2 :: Int
    let real_valid_operation = case valid_operation of
                                 "+" -> (+)
                                 "-" -> (-)
                                 "*" -> (-)

    let solution = real_valid_operation real_num1 real_num2
    print solution

-- Q4
infiniteAppend :: IO ()
infiniteAppend = forever appendToFile
    where appendToFile = do
            path <- getLine
            line <- getLine
            appendFile path line

-- Monads
-- Q1
flattenDo :: [[a]] -> [a]
flattenDo xss = do
    xs <- xss
    x <- xs
    return x

flattenMonad :: [[a]] -> [a]
flattenMonad xss = xss >>= \xs -> xs >>= \x -> return x

-- Q2

superSafeDiv :: Int -> Int -> Maybe Int
superSafeDiv x 0 = Nothing
superSafeDiv x y = if x `mod` y == 0 
                    then Just (x `div` y) 
                    else Nothing

-- Q3
divTwice :: Int -> Int -> Maybe Int
divTwice x y = superSafeDiv x y >>= \z -> superSafeDiv z y

-- Connected words
connectionMap :: [(Char, [Char])]
connectionMap =
    [
        ('a', "aqzsw"),
        ('b', "bvghn"),
        ('c', "cxdfv"),
        ('d', "dxserfc"),
        ('e', "ewsdr"),
        ('f', "fdrtgvc"),
        ('g', "gftyhbv"),
        ('h', "hgyujnb"),
        ('i', "iujklo"),
        ('j', "jhuikmn"),
        ('k', "kjiolm"),
        ('l', "lkop"),
        ('m', "mnjk"),
        ('n', "nbhjm"),
        ('o', "oiklp"),
        ('p', "pol"),
        ('q', "qwa"),
        ('r', "redft"),
        ('s', "sawedxz"),
        ('t', "trfgy"),
        ('u', "uyhji"),
        ('v', "vcfgb"),
        ('w', "wqase"),
        ('x', "xzsdc"),
        ('y', "ytghu"),
        ('z', "zasx")
    ]

-- Q1
isConnected :: String -> Bool
isConnected word = undefined

-- Q2
connectedWords :: IO [String]
connectedWords = undefined

-- Q3
printStats :: IO ()
printStats = undefined
