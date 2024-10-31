import Data.List (intercalate)

takeThose :: (a->Bool) -> [a] -> [a]
takeThose f = foldr (\x acc -> if f x then x : acc else acc) []

ntimes :: [Char] -> Int -> [Char]
ntimes word 0 = word
ntimes word n = word ++ ntimes word (n-1)

prop_ntimesLength word n = length (ntimes word n) == n * length word

data Dinosaur = Dinosaur Bool Bool Int deriving Eq

instance Show Dinosaur where
    show (Dinosaur winged spikey legs) = if winged then ">" ++ body ++ "<" else body
       where
            buildBody 0 = ""
            buildBody 1 = "b"
            buildBody n = "b" ++ separator ++ buildBody (n - 1)

            separator = if spikey then "^" else "-"
        
            body = buildBody legs