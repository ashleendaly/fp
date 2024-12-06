module Game where
import Deck
import Error

{- Commands and instructions, representing moves to be made -}
type StackIndex = Int
type Count = Int
type FromStack = StackIndex
type ToStack = Int

-- An instruction is a top-level command.
data Instruction = Quit | Undo | GameCommand Command

-- A Command is a move to be played in the game.
data Command = Move Count FromStack ToStack
             | MoveStack FromStack ToStack
             | Draw
             | MoveFromDiscard StackIndex
             | MoveToPillar CardSource
             | MoveFromPillar Suit StackIndex
             | Solve

data CardSource = FromStack StackIndex | FromDiscard

{- Board representation -}

-- A column is a list of (Card, Bool) pairs, where the Bool
-- represents visibility: true for visible, false for hidden
type Column = [(Card, Bool)]

-- The pillars / foundation stacks are represented as Maybe Card
-- values, where Nothing represents an empty pillar and Just c 
-- denotes that card 'c' is at the top of the pillar.
-- Note that we don't need to store all cards since they are stored
-- in ascending order.
data Pillars = MkPillars {
        spades :: Maybe Value,
        clubs :: Maybe Value,
        hearts :: Maybe Value,
        diamonds :: Maybe Value
  }
  deriving (Show, Eq)

emptyPillars :: Pillars
emptyPillars = MkPillars {
        spades = Nothing,
        clubs = Nothing,
        hearts = Nothing,
        diamonds = Nothing
    }

-- The board consists of a deck, discard pile, pillars, and 7 columns.
data Board = MkBoard {
    boardDeck :: [Card],
    boardDiscard :: [Card],
    boardPillars :: Pillars,
    boardColumns :: [Column]
}
    deriving (Eq)



{- EXERCISE 3: Show instance for the board -}
{- We recommend writing helper functions. -}
instance Show Board where
    show b =
        unlines [
            deckSize,
            discard,
            pillars,
            columns
        ]
        where
            deckSize = "Deck size: " ++ show (length (boardDeck b))
            discard = "Discard: " ++ showRecent (boardDiscard b)
            pillars = "Pillars:\n" ++ showPillars (boardPillars b)
            columns = showColumns (boardColumns b)

            takeLast :: Int -> [a] -> [a]
            takeLast n xs = drop (length xs - n) xs

            showRecent :: [Card] -> String
            showRecent discard = foldr (\card acc -> show card ++ (if null acc then "" else ", ") ++ acc) "" (takeLast 3 discard)

            showPillars :: Pillars -> String
            showPillars ps = unlines [
                "Spades: " ++ showCard (spades ps) Spades,
                "Clubs: " ++ showCard (clubs ps) Clubs,
                "Hearts: " ++ showCard (hearts ps) Hearts,
                "Diamonds: " ++ showCard (diamonds ps) Diamonds
                ]

            showCard :: Maybe Value -> Suit -> String
            showCard Nothing _ = "<empty>"
            showCard (Just value) suit = show suit ++ show value

            showColumns :: [Column] -> String
            showColumns columns = unlines (map unwords indexedCols)
            -- showColumns columns = unlines (map unwords transposed)
                where
                    indexedCols = zipWith showColumn [0..] columns
                    -- transposed = transpose indexedCols

            showColumn :: Int -> Column -> [String]
            showColumn idx cards = header : cardsStr
                where
                    header = "[" ++ show idx ++ "]"
                    cardsStr = map showCardInColumn cards

            showCardInColumn :: (Card, Bool) -> String
            showCardInColumn (card, visible) = if visible then show card else "???"

            transpose :: [[String]] -> [[String]]
            transpose [] = []
            transpose ([] : xs) = transpose xs
            transpose ((y : ys) : xs) = (y : map head xs) : transpose (ys : map tail xs)

{- EXERCISE 4: Board Setup -}
setup :: Deck -> Board
setup d = MkBoard {
    boardDeck = remainingDeck, -- cards not used in the board set up
    boardDiscard = [],
    boardPillars = emptyPillars,
    boardColumns =  initialColumns
}
    where
        numColumns = 7
        initialColumns = dealColumns numColumns d
        remainingDeck = drop (sum [1..numColumns]) d

        dealColumns :: Int -> [Card] -> [Column]
        dealColumns n deck = dealHelper n deck 0 []

        dealHelper :: Int -> [Card] -> Int -> [Column] -> [Column]
        dealHelper 0 _ _ columns = columns
        dealHelper n deck currentCount columns =
            dealHelper (n - 1) rest (currentCount + 1) (columns ++ [column])
            where
                (columnCards, rest) = splitAt (currentCount + 1) deck
                column = makeColumn columnCards

        makeColumn :: [Card] -> Column
        makeColumn [] = []
        makeColumn cards = faceDown ++ faceUp
            where
                faceDown = map (\card -> (card, False)) (init cards)
                faceUp = [(last cards, True)]



{- EXERCISE 5: Win checking -}
isWon :: Board -> Bool
isWon b = all isKing [
    getPillar ps Spades,
    getPillar ps Clubs,
    getPillar ps Hearts,
    getPillar ps Diamonds
    ]
    where
        ps = boardPillars b

        isKing :: Maybe Value -> Bool
        isKing (Just King) = True
        isKing _ = False

{- Pillar helper functions -}
-- Gets the pillar for a given suit.
getPillar :: Pillars -> Suit -> Maybe Value
getPillar ps Spades = spades ps
getPillar ps Clubs = clubs ps
getPillar ps Hearts = hearts ps
getPillar ps Diamonds = diamonds ps

-- Decrements a pillar. 
decValue :: Maybe Value -> Maybe Value
decValue Nothing = Nothing
decValue (Just Ace) = Nothing
decValue (Just x) = Just (pred x)

-- Increments a pillar.
incValue :: Maybe Value -> Maybe Value
incValue Nothing = Just Ace
incValue (Just x) = Just (succ x)

-- Increments the pillar for a given suit.
incPillar :: Pillars -> Suit -> Pillars
incPillar ps Spades = ps { spades = incValue (spades ps) }
incPillar ps Clubs = ps { clubs = incValue (clubs ps) }
incPillar ps Hearts = ps { hearts = incValue (hearts ps) }
incPillar ps Diamonds = ps { diamonds = incValue (diamonds ps) }

-- Decrements the pillar for a given suit.
decPillar :: Pillars -> Suit -> Pillars
decPillar ps Spades = ps { spades = decValue $ spades ps }
decPillar ps Clubs = ps { clubs = decValue $ clubs ps }
decPillar ps Hearts = ps { hearts = decValue $ hearts ps }
decPillar ps Diamonds = ps { diamonds = decValue $ diamonds ps }

{- EXERCISE 6: Helper Functions -}

-- Flips the top card of all columns, if not already flipped
flipCards :: Board -> Board
flipCards b = b {boardColumns = map flipTopCard (boardColumns b)}
    where
        flipTopCard :: Column -> Column
        flipTopCard [] = []
        flipTopCard cards = reverse ((fst (head reversedCards), True) : tail reversedCards)
            where
                reversedCards = reverse cards


-- Checks whether it's possible to stack the first card onto the second.
canStack :: Card -> Card -> Bool
canStack card onto = (isRed card && isBlack onto || isBlack card && isRed onto) &&
                    cardValue card == pred (cardValue onto)

-- Updates a column at the given index
updateColumn :: Int -> Column -> [Column] -> [Column]
updateColumn n c cs = take n cs ++ [c] ++ drop (n + 1) cs

-- Checks whether it's possible to place a card onto a pillar.
canStackOnPillar :: Card -> Maybe Value -> Bool
canStackOnPillar c mv =
    case mv of
        Nothing -> cardValue c == Ace
        Just value -> cardValue c == succ value

{- EXERCISE 7: Draw -}
draw :: Board -> Either Error Board
draw b
    | not (null deck) = Right b {boardDeck = take (length deck -1) deck, boardDiscard = discard ++ [last deck]}
    | null deck && not (null discard) = draw b {boardDeck = reverse discard, boardDiscard = []}
    | otherwise = Left DeckEmpty
    where
         deck = boardDeck b
         discard = boardDiscard b

{- EXERCISE 8: Move -}
move :: Int -> Int -> Int -> Board -> Either Error Board
move count from to b
    | count < 1 = Left InvalidCount
    | count > length visibleCards = Left MovingTooManyCards
    | null toCol && cardValue (fst (head toMove)) /= King = Left ColumnKing
    | not (null toCol) && not (canStack (fst (head toMove)) (fst (last toCol))) = Left WrongOrder
    | otherwise = Right $ b {
            boardColumns = newColumns'
        }
    where
        columns = boardColumns b
        fromCol = columns !! from
        toCol = columns !! to

        visibleCards = filter snd fromCol

        (newFromCol, toMove) = splitAt (length fromCol - count) fromCol
        newColumns = updateColumns columns from newFromCol
        newColumns' = updateColumns newColumns to (toCol ++ toMove)

updateColumns :: [Column] -> Int -> Column -> [Column]
updateColumns cols idx col = take idx cols ++ [col] ++ drop (idx+1) cols

{- EXERCISE 9: Move Stack -}
moveStack :: Int -> Int -> Board -> Either Error Board
moveStack from to b = move stack from to b
  where
    columns = boardColumns b
    fromCol = columns !! from
    stack = length (filter snd fromCol)

{- EXERCISE 10: Move from Discard -}
moveFromDiscard :: Int -> Board -> Either Error Board
moveFromDiscard idx b 
    | null discard = Left DiscardEmpty
    | null toCol && cardValue topOfDiscard /= King = Left ColumnKing
    | not (null toCol) && not (canStack topOfDiscard (fst (last toCol))) = Left WrongOrder
    | otherwise = Right $ b {
            boardDiscard = take ((length discard)-1) discard,
            boardColumns = updateColumns columns idx (toCol ++ [(topOfDiscard, True)])
        }
    where
        discard = boardDiscard b
        columns = boardColumns b
        toCol = columns !! idx
        
        topOfDiscard = last discard

{- EXERCISE 11: Move to Pillar -}
moveToPillar :: CardSource -> Board -> Either Error Board
moveToPillar cs b =
    case cs of
        FromDiscard ->
            if null discard
                then Left DiscardEmpty
                else tryMoveToPillar (last discard) b (\b -> b { boardDiscard = take ((length discard)-1) discard })
        FromStack idx ->
            if null fromCol
                then Left ColumnEmpty
                else tryMoveToPillar (fst (last fromCol)) b (\b -> b { boardColumns = updatedColumns })
            where
                fromCol = columns !! idx
                updatedColumn = take ((length fromCol)-1) fromCol
                updatedColumns = updateColumns columns idx updatedColumn
    where
        discard = boardDiscard b
        columns = boardColumns b
        

        tryMoveToPillar :: Card -> Board -> (Board -> Board) -> Either Error Board
        tryMoveToPillar card b updateFn = 
            if canStackOnPillar card pillar
                then Right $ updateFn b { boardPillars = incPillar (boardPillars b) suit }
                else Left WrongPillarOrder
            where
                suit = cardSuit card
                pillar = getPillar (boardPillars b) suit


{- EXERCISE 12: Move from Pillar -}
moveFromPillar :: Suit -> Int -> Board -> Either Error Board
moveFromPillar suit idx b
    | pillarValue == Nothing = Left PillarEmpty
    | cardValue pillarCard /= King && null toCol = Left ColumnKing
    | not (null toCol) && not (canStack pillarCard (fst (last toCol))) = Left WrongOrder
    | otherwise = Right $ b {
            boardPillars = decPillar (boardPillars b) suit,
            boardColumns = updateColumns columns idx (toCol ++ [(pillarCard, True)])
    }
    where
        columns = boardColumns b
        toCol = columns !! idx
        pillarValue = getPillar (boardPillars b) suit
        pillarCard = case pillarValue of
            Just value -> MkCard suit value
            Nothing -> error "Pillar was empty, this should be caught earlier"


{- EXERCISE 13: Solve -}
solve :: Board -> Board
solve board = 
    let newBoard = trySolve board 0
    in if board == newBoard
           then board
           else solve newBoard
    where
        newBoard = trySolve board

        trySolve :: Board -> Int -> Board
        trySolve b 8 = b
        trySolve b n =
            let updatedBoard = tryMoveLastCardInColumn b n
            in trySolve updatedBoard (n + 1)

        tryMoveLastCardInColumn :: Board -> Int -> Board
        tryMoveLastCardInColumn b n
                | n < 0 || n >= length columns = b
                | null col = b
                | canStackOnPillar lastCard (getPillar ps suit) =
                    b {
                        boardPillars = incPillar ps suit,
                        boardColumns = updateColumn n (init col) columns
                    }
                | otherwise = b
            where
                columns = boardColumns b
                col = columns !! n
                ps = boardPillars b

                lastCard = fst (last col)
                suit = cardSuit lastCard



{- Scaffolding: This checks input indexes and calls the relevant functions -}
checkStackIndex :: Int -> Either Error ()
checkStackIndex x | x >= 0 && x <= 6 = return ()
                  | otherwise = Left InvalidStack

makeMove' :: Command -> Board -> Either Error Board
makeMove' (Move count from to) b = do
    checkStackIndex from
    checkStackIndex to
    move count from to b
makeMove' (MoveStack from to) b = do
    checkStackIndex from
    checkStackIndex to
    moveStack from to b
-- If deck nonempty, move a card from the top of the deck to the top of the discard pile
-- If deck empty, reverse discard pile and put it back as deck
makeMove' Draw b = draw b
makeMove' (MoveFromDiscard idx) b = checkStackIndex idx >> moveFromDiscard idx b
-- Take the top card from the given stack and move to pillar -- if possible
makeMove' (MoveToPillar source) b =
    case source of
        FromDiscard -> moveToPillar source b
        FromStack idx -> checkStackIndex idx >> moveToPillar source b
makeMove' (MoveFromPillar suit idx) b = checkStackIndex idx >> moveFromPillar suit idx b
makeMove' Solve b = Right $ solve b

makeMove :: Command -> Board -> Either Error Board
makeMove cmd b = fmap flipCards (makeMove' cmd b)
