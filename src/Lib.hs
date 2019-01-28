
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

first :: (a,b) -> a
first (x, _) = x

second :: (a, b) -> b
second (_, x) = x

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
    nextXHeadPos = (first . head . body . snake $ gs) +
                   (first . mapDirection . direction . snake $ gs)
    nextYHeadPos = (second . head . body . snake $ gs) +
                   (second . mapDirection . direction . snake $ gs)


checkBodyCollision :: Snake -> Bool
checkBodyCollision = undefined
