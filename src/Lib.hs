
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

checkWallCollision :: GameState -> Bool
checkWallCollision gs =
  (nextYHeadPos <= topWall && nextYHeadPos >= bottomWall) &&
  (nextXHeadPos <= rightWall) && (nextXHeadPos >= leftWall) where
    topWall = height gs
    bottomWall = 0
    rightWall = width gs
    leftWall = 0
    nextXHeadPos = (fst . head . body . snake $ gs) +
                   (fst . mapDirection . direction . snake $ gs)
    nextYHeadPos = (snd . head . body . snake $ gs) +
                   (snd . mapDirection . direction . snake $ gs)


checkBodyCollision :: Snake -> Bool
checkBodyCollision s = overlap (nextXHeadPos, nextYHeadPos) (body s) where
  nextXHeadPos = (fst . head . body $ s) +
                 (fst . mapDirection . direction $ s)
  nextYHeadPos = (snd . head . body $ s) +
                 (snd . mapDirection . direction $ s)
  overlap _ (x:[]) = False
  overlap h (b:bs)
    | h == b = True
    | otherwise = overlap h bs
