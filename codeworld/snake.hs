import CodeWorld
import Data.List
import Data.Text(pack, unpack)
import System.Random

type RandomNumber = Int
gridSize = 0.5 :: Double
gridSizeHf = 0.25 :: Double

main :: IO ()
main = do
    gen <- getStdGen
    activityOf (initialWorld gen) handleEvent drawWorld
    
data GameState = GameIdle
               | GameRunning
               | GameOver
               deriving(Show, Eq)

data World = World {
    rnds  :: [RandomNumber],
    state :: GameState,
    snake :: Snake,
    snakeM :: Snake,
    apple :: Apple,
    arenaWidth :: Double,
    arenaHeight :: Double
    }

initialWorld :: StdGen -> World
initialWorld gen = restartWorld rnds width height
    where rnds = randomRs (round (0 - width), round (width - 2)) gen
          width = 20 :: Double
          height = 20 :: Double
          
restartWorld :: [RandomNumber] -> Double -> Double -> World
restartWorld rnds w h = World rnds' GameRunning snake snakeM apple w h
    where snake = mkSnake (-3, 0) 3 pink
          snakeM = mkSnake (3, 0) 3 blue
          apple = mkApple ((fromIntegral r1) / 2, (fromIntegral r2) / 2) green
          (r1:r2:rnds') = rnds
          
drawWorld :: World -> Picture
drawWorld world
  | state world == GameOver = draw world
                            & translated 0 (1)
                             (styledLettering Bold Handwriting
                                              (pack "Game Over!"))
                            & translated 0 (-1)
                             (styledLettering Bold Handwriting
                                              (pack score1_str))
                            & translated 0 (-2.5)
                             (styledLettering Bold Handwriting
                                              (pack score2_str))
  | otherwise   = draw world
    where draw world = drawSnake (snake world)
                     & drawSnake (snakeM world)
                     & drawApple (apple world)
                     & rectangle (arenaWidth world) (arenaHeight world)
          score1_str = "Player1 Score: " ++ (show $ score $ snake world)
          score2_str = "Player2 Score: " ++ (show $ score $ snakeM world)

handleEvent :: Event -> World -> World
handleEvent (TimePassing dt) w
  | state w == GameOver = w
  | otherwise = handleSnakeAction snake' action
              . handleSnakeMAction snakeM' actionM
              $ w
    where (snake', action) = checkSnakeAction dt w (snake w)
          (snakeM', actionM) = checkSnakeAction dt w (snakeM w)

handleEvent (KeyPress keyText) w
  | state w == GameOver
    && unpack keyText == "Enter" = restartWorld (rnds w) arenaW arenaH
  | unpack keyText == "Up"    = w {snake = turnSnake DirectUp snake'}
  | unpack keyText == "Down"  = w {snake = turnSnake DirectDown snake'}  
  | unpack keyText == "Left"  = w {snake = turnSnake DirectLeft snake'}
  | unpack keyText == "Right" = w {snake = turnSnake DirectRight snake'}
  | unpack keyText == "W"  = w {snakeM = turnSnake DirectUp snakeM'}
  | unpack keyText == "S"  = w {snakeM = turnSnake DirectDown snakeM'}  
  | unpack keyText == "A"  = w {snakeM = turnSnake DirectLeft snakeM'}
  | unpack keyText == "D"  = w {snakeM = turnSnake DirectRight snakeM'}
  | otherwise = w
    where snake' = snake w
          snakeM' = snakeM w
          arenaW = arenaWidth w
          arenaH = arenaHeight w
handleEvent _ w = w

handleSnakeAction snake' action w
  | action == SnakeMove = w {snake = moveSnake snake'}
  | action == SnakeEat  = w {snake = eatingSnake snake',
                             apple = apple',
                             rnds  = rnds'}
  | action == SnakeNoAct = w {snake = snake'}
  | action == SnakeDead  = w {state = GameOver}
    where (apple', rnds') = randomApple w

handleSnakeMAction snake' action w
  | action == SnakeMove = w {snakeM = moveSnake snake'}
  | action == SnakeEat  = w {snakeM = eatingSnake snake',
                             apple = apple',
                             rnds  = rnds'}
  | action == SnakeNoAct = w {snakeM = snake'}
  | action == SnakeDead  = w {state = GameOver}
    where (apple', rnds') = randomApple w

-- Check the selected snake if it should be move or collision some things.
-- If the snake capture a apple then eat it
-- If the snake collision the wall or self, other snake, then game over.
-- If nothing can be touched by snake, then move alone with direction of self
checkSnakeAction :: Double -> World -> Snake
                 -> (Snake, SnakeAction)
checkSnakeAction dt world snakeS
    = if ds' > snkW then (snakeS{ds = 0}, action)
                    else (snakeS{ds = ds'}, action1)
    where ds' = ds snakeS + dt * snakeSpeed
          snkW  = width snakeS
          headOrg = head $ bodyPoints snakeS
          headN = translatedPoint dx dy headOrg
          (dx, dy) = getSnakeDxDy snakeS

          appleOrg = apple world
          (snkHead:snkTail)   = bodyPoints $ snake world
          (snkMHead:snkMTail) = bodyPoints $ snakeM world
          
          action = if headN == position appleOrg then SnakeEat else SnakeMove
          action1 = if   headOrg `elem` snkTail
                      || headOrg `elem` snkMTail
                      || outofBound headOrg
                    then SnakeDead
                    else SnakeNoAct
          outofBound (x, y) = if   x < 0 - maxW || x > maxW - gridSize
                                || y < 0 - maxH || y > maxH - gridSize
                              then True
                              else False
          maxW = arenaWidth world / 2
          maxH = arenaHeight world / 2
          
data Apple = Apple {
    position :: Point,
    colorA   :: Color,
    widthA   :: Double
    }

mkApple :: Point -> Color -> Apple
mkApple pos color = Apple pos color 0.25

drawApple :: Apple -> Picture
drawApple apple@(Apple pos color width)
    = translated (x + gridSizeHf) (y + gridSizeHf)
    $ colored color
    $ solidCircle width
    where (x, y) = pos

randomApple world@(World rnds _ _ _ apple w h) = (mkApple pos color, rnds')
    where color = if colorOrg == red then green else red
          pos = ((fromIntegral r1) / 2, (fromIntegral r2) / 2)
          (r1:r2:rnds') = rnds
          colorOrg = colorA apple

data Direction = DirectUp
               | DirectDown
               | DirectLeft
               | DirectRight
               deriving(Show, Eq, Ord)

data SnakeAction = SnakeNoAct
                 | SnakeMove
                 | SnakeEat
                 | SnakeDead
                 deriving(Show, Eq)

data Snake = Snake {
    bodyPoints :: [Point],
    ds     :: Double,     -- acceleration distance by passing time
    score  :: Int,
    direct :: Direction,
    color  :: Color,
    width  :: Double
    }
    
snakeSpeed = 2

mkSnake :: Point -> Int -> Color -> Snake
mkSnake startPoint len color = Snake body 0 0 DirectUp color w
    where body = unfoldr (\i -> if i < len
                                then Just ((x, y - w * fromIntegral i), i + 1)
                                else Nothing)
                         0
          (x, y) = startPoint
          w      = gridSize  -- width of snake
    
drawSnake :: Snake -> Picture
drawSnake snake = foldr1 (&) blks
    where blks = map (colored snkColor . drawBodyBlk) snkBody
          snkColor = color snake
          snkBody = bodyPoints snake
          drawBodyBlk (x, y) = translated (x + gridSizeHf) (y + gridSizeHf)
                             $ solidRectangle (w - 0.05) (w - 0.05)
          w = width snake

turnSnake :: Direction -> Snake -> Snake
turnSnake dir snake = if isConflictDirect dir (direct snake)
                      then snake
                      else snake { direct = dir }
    where isConflictDirect direct1 direct2
            | direct1 == DirectUp && direct2 == DirectDown = True
            | direct1 == DirectDown && direct2 == DirectUp = True
            | direct1 == DirectLeft && direct2 == DirectRight = True
            | direct1 == DirectRight && direct2 == DirectLeft = True
            | otherwise = False

moveSnake :: Snake -> Snake
moveSnake snake = snake {ds = 0, bodyPoints = pts}
    where ptsOrg = bodyPoints snake
          pts = pt : init ptsOrg
          pt = translatedPoint dx dy $ head ptsOrg
          (dx, dy) = getSnakeDxDy snake

eatingSnake :: Snake -> Snake
eatingSnake snake = snake {ds = 0, score = score', bodyPoints = pts}
    where ptsOrg = bodyPoints snake
          pts = pt : ptsOrg
          pt = translatedPoint dx dy $ head ptsOrg
          (dx, dy) = getSnakeDxDy snake
          score' = score snake + 5

{-# INLINE getSnakeDxDy #-}
getSnakeDxDy snake@(Snake _ _ _ direct _ w)
  | direct == DirectUp    = (0, w)
  | direct == DirectDown  = (0, -w)
  | direct == DirectLeft  = (-w, 0)
  | direct == DirectRight = (w, 0)

