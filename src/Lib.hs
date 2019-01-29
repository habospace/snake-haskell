
data Direction =
    Up
  | Down
  | Left'
  | Right' deriving (Eq, Show)

data Snake = Snake {
    body :: [(Int, Int)],
    direction :: Direction
  } deriving (Eq, Show)

data GameState = GameState {
    width :: Int,
    height :: Int,
    snake :: Snake,
    score :: Int,
    food :: (Int, Int)
  } deriving (Eq, Show)

mapDirection :: Direction -> (Int, Int)
mapDirection x
  | x == Up    = (0, 1)
  | x == Down  = (0, -1)
  | x == Left' = (1, 0)
  | otherwise  = (-1, 0)

getNextSnakeHeadPos :: Snake -> (Int, Int)
getNextSnakeHeadPos s = (nextXHeadPos, nextYHeadPos) where
  nextXHeadPos = (fst . head . body $ s) +
                 (fst . mapDirection . direction $ s)
  nextYHeadPos = (snd . head . body $ s) +
                 (snd . mapDirection . direction $ s)

checkWallCollision :: GameState -> Bool
checkWallCollision gs =
  (snd nextHeadPos >= topWall && snd nextHeadPos <= bottomWall) &&
  (fst nextHeadPos >= rightWall) && (fst nextHeadPos <= leftWall) where
    topWall = height gs
    bottomWall = 0
    rightWall = width gs
    leftWall = 0
    nextHeadPos = getNextSnakeHeadPos . snake $ gs

checkBodyCollision :: Snake -> Bool
checkBodyCollision s = overlap (fst nextHeadPos, snd nextHeadPos) (body s) where
  nextHeadPos = getNextSnakeHeadPos s
  overlap _ []     = False
  overlap _ (x:[]) = False
  overlap h (b:bs)
    | h == b = True
    | otherwise = overlap h bs

moveSnake :: Snake -> Snake
moveSnake s = Snake (nextHeadPos:(newTail . body $ s)) (direction s) where
  nextHeadPos = getNextSnakeHeadPos s
  newTail []     = []
  newTail (x:[]) = []
  newTail (x:xs) = x : newTail xs

consumeFood :: Snake -> Snake
consumeFood s = Snake (nextHeadPos:(body s)) (direction s) where
  nextHeadPos = getNextSnakeHeadPos s

