import Data.Char (toUpper)
import Data.IORef
import Debug.Trace (trace)
import Data.UnixTime (getUnixTime, utSeconds)
import Foreign.C.Types
import System.Random

{- Console IO -}
getAndPrintReverse :: IO ()
getAndPrintReverse = do
  str <- getLine
  let revStr = reverse str
  putStrLn revStr

makeUpper :: String -> String
makeUpper = map toUpper

main :: IO ()
main = do
  line <- getLine
  putStrLn (makeUpper line)

{- Random number generation -}

genRandoms :: StdGen -> Int -> [Int]
genRandoms gen n | n <= 0 = []
                 | otherwise =
                     let (x, gen') = randomR (1 :: Int, 10 :: Int) gen in
                     x : genRandoms gen' (n - 1)

seedRNG :: IO StdGen
seedRNG = do
    time <- getUnixTime
    let (CTime seconds) = utSeconds time
    return (mkStdGen (fromEnum seconds))

seedAndGenerate :: Int -> IO [Int]
seedAndGenerate n = do
  gen <- seedRNG
  return (genRandoms gen n)

{- IO References -}
incrementRef :: IORef Int -> IO ()
incrementRef ref = do
  contents <- readIORef ref
  writeIORef ref (contents + 1)

incrementThrice :: IO ()
incrementThrice = do
  ref <- newIORef 0
  incrementRef ref
  incrementRef ref
  incrementRef ref
  contents <- readIORef ref
  putStrLn (show contents)