
data Wrap a = Wrap a deriving (Eq, Show)

instance Functor Wrap where
  fmap f (Wrap x) = Wrap (f x)

instance Applicative Wrap where
  pure = Wrap
  (<*>) (Wrap f) (Wrap x) = Wrap (f x)

instance Monad Wrap where
  return = pure
  (>>=) (Wrap x) f = f x

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
    food :: (Int, Int),
    justAte :: Bool,
    inGame :: Bool
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

checkFoodConsumption :: GameState -> Bool
checkFoodConsumption gs = (food gs) == (head . body . snake $ gs)

moveSnake :: Snake -> Snake
moveSnake s = Snake (nextHeadPos:(newTail . body $ s)) (direction s) where
  nextHeadPos = getNextSnakeHeadPos s
  newTail []     = []
  newTail (x:[]) = []
  newTail (x:xs) = x : newTail xs

consumeFood :: Snake -> Snake
consumeFood s = Snake (nextHeadPos:(body s)) (direction s) where
  nextHeadPos = getNextSnakeHeadPos s

--updateGameAfterMoving :: GameState -> GameState
--updateGameAfterMoving gs =
--    GameState (width gs) (height gs) (consumeFood . snake $ gs)
--              ((+1) . score $ gs) (width gs, height gs) True True

updateGameAfterEating :: GameState -> GameState
updateGameAfterEating gs =
  GameState (width gs) (height gs) (consumeFood . snake $ gs)
            ((+1) . score $ gs) (width gs, height gs) True True

setInGameState :: GameState -> Bool -> GameState
setInGameState gs igs =
  GameState (width gs) (height gs) (snake gs)
            (score gs) (food gs) (justAte gs) igs

{-tickGame :: GameState -> Wrap GameState
tickGame gs = do
  gs1 <- (\x -> if (checkWallCollision x) || (checkBodyCollision . snake $ x) then return (setInGameState x False) else return x) gs
  gs2 <- (\x -> if ((inGame x) /= False) && (checkFoodConsumption x) then return (updateGameAfterEating x) else return x) gs1
  (\x -> if (inGame x) == True then return (moveSnake x) else return x) gs2
-}

