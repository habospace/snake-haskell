module Lib where

import System.Random

type X = Int
type Y = Int
type DeltaX = Int
type DeltaY = Int
type Location = (X, Y)
type Movement = (DeltaX, DeltaY)
type SnakeBody  = [Location]
type FoodSpawnings = [Location]

data Direction =
    North
  | South
  | West
  | East deriving (Eq, Show) 

data Snake = Snake {
    body             :: SnakeBody,
    nextDirection    :: Direction,
    currentDirection :: Direction 
  } deriving (Eq, Show)

data GameState = GameState {
    gameWidth  :: Int,
    gameHeight :: Int,
    snake      :: Snake,
    score      :: Int,
    food       :: Location,
    foods      :: FoodSpawnings,
    gameOver   :: Bool
  } deriving (Eq, Show)

initGame :: Int -> Int -> Direction -> GameState
initGame w h d = spawnFood gs where
    gs = GameState w h snake 0 (0, 0) fspwns False 
    snake = Snake [(w `div` 2, h `div` 2)] d d
    fspwns = zip xs ys 
    xs = (max 1) <$> ((mod <$> (randoms (mkStdGen 2))) <*> (pure w))
    ys = (max 1) <$> ((mod <$> (randoms (mkStdGen 3))) <*> (pure h))

isIn :: (Foldable f, Eq a) => f a -> a -> Bool
isIn xs x = foldr (\x' acc -> (x' == x || acc)) False xs
    
isInSnakeBody = isIn
checkCollision = isIn

removeLast :: [a] -> [a]
removeLast [] = []
removeLast (x:[]) = []
removeLast (x:xs) = x : removeLast xs

mapDirection :: Direction -> Movement
mapDirection d = case d of
  North -> (0, 1)
  South -> (0, -1)
  West  -> (-1, 0)
  East  -> (1, 0)

spawnFood :: GameState -> GameState
spawnFood gs@(GameState _ _ s@(Snake body _ _) _ _ (f:fs) _) = gs' where
  gs' = if isInSnakeBody body f
    then spawnFood $ gs {foods = fs}
    else gs {food = f, foods = fs}

tick :: GameState -> GameState
tick gs@(GameState w h (Snake body@((x, y):tail) d _) sc f fs gOver)
    | gOver     = gs
    | otherwise = if justAte then spawnFood gs' else gs' where
        gs'      = GameState w h (Snake body' d d) sc' f fs gOver'
        gOver'   = checkCollision tail head 
        body'    = head : if justAte then body else removeLast body
        sc'      = if justAte then sc + 1 else sc
        justAte  = head == f  
        head     = repositionSnakeHead w h $ (x+dx, y+dy)
        (dx, dy) = mapDirection d

repositionSnakeHead :: Int -> Int -> Location -> Location
repositionSnakeHead w h (x, y) = (x', y') where
  x' = if 1 <= x && x <= w then x else 
      if x > w 
        then x - w
        else x + w
  y' = if 1 <= y && y <= h then y else 
      if y > h  
        then y - h
        else y + h

changeDirection :: Direction -> GameState -> GameState
changeDirection d gs@(GameState _ _ s@(Snake _ _ d') _ _ _ _) =
  case d of 
    South -> if d' /= North then gs {snake = s {nextDirection = d}} else gs
    North -> if d' /= South then gs {snake = s {nextDirection = d}} else gs
    East  -> if d' /= West then gs {snake = s {nextDirection = d}} else gs
    West  -> if d' /= East then gs {snake = s {nextDirection = d}} else gs
