type Position = (Int, Int)
data Color = White | Black deriving (Eq, Show)
data Peg = Peg Position Color deriving (Eq, Show)
data Move = Move Position deriving (Eq, Show)
type Board = [Peg]
data State = State Move Board deriving (Eq, Show)

createBoard :: Position -> Board
createBoard (x, y)
    | (abs x <= 3 && abs y <= 3 && (x, y) /= (-3, -2) && (x, y) /= (-3, -3) && (x, y) /= (-2, -2) && (x, y) /= (-3, 2) && (x, y) /= (2, 2) && (x, y) /= (2, -2) && (x, y) /= (2, -3)) = board
    | otherwise = error "The position is not valid."
  where
    board = createLists x y

createLists :: Int -> Int -> [Peg]
createLists x y = whitePegs ++ blackPegs
  where
    whitePegs = createWhitePegs x y
    blackPegs = createBlackPegs x y

createWhitePegs :: Int -> Int -> [Peg]
createWhitePegs x y = [Peg (i, j) White | i <- [0..x], j <- [0..y], (i, j) `notElem` [(-3, -2), (-3, -3), (-2, -2), (2, 2), (2, -2), (3, -3)]

createBlackPegs :: Int -> Int -> [Peg]
createBlackPegs x y = [Peg (i, j) Black | i <- [0..x], j <- [0..y], (i, j) `elem` [(-3, -2), (-3, -3), (-2, -2), (2, 2), (2, -2), (3, -3)]
  

isValidMove :: Move -> Board -> Bool
isValidMove (Move (x, y)) board = any (\(Peg (i, j) color) -> color == White && any (flipPeg (x, y)) [Peg (i+1, j) White, Peg (i-1, j) White, Peg (i, j+1) White, Peg (i, j-1) White]) board
  where
    flipPeg :: Position -> Peg -> Bool
    flipPeg pos (Peg pegPos color) = pos == pegPos && color == White

isGoal :: Board -> Bool
isGoal board = all (\(Peg _ color) -> color == White) board

showPossibleNextStates :: Board -> [State]
showPossibleNextStates board
    | isGoal board = error "No possible States Exist."
    | otherwise = [State (Move (x, y)) (flipPeg (x, y) board) | Peg (x, y) Black <- board, isValidMove (Move (x, y)) board]
  where
    flipPeg :: Position -> Board -> Board
    flipPeg pos ((Peg pegPos color):t) | pegPos == pos = (Peg pegPos White):t
                                        | otherwise = (Peg pegPos color):(flipPeg pos t)