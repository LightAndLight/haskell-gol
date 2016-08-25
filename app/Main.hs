module Main where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.State
import           Data.Char
import           Data.Foldable
import           Data.Monoid
import           System.IO

width = 10
height = 10

posMod :: Int -> Int -> Int
posMod a b = let res = a `mod` b
             in if res < 0 then res + b else res

set :: Int -> a -> [a] -> [a]
set n x' [] = []
set 0 x' (x:xs) = x':xs
set n x' (x:xs) = x:set (n - 1) x' xs

data Cell = Alive | Dead deriving (Eq, Show)

data Row = Row { numCells :: Int, cells :: [Cell] } deriving (Eq, Show)

getCell :: Int -> Row -> Cell
getCell n (Row w cells) = cells !! (n `posMod` w)

setCell :: Int -> Cell -> Row -> Row
setCell n cell' (Row w cells) = Row w $ set (n `posMod` w) cell' cells

data Grid = Grid { numRows :: Int, rows :: [Row] } deriving (Eq, Show)

getRow :: Int -> Grid -> Row
getRow n (Grid h rows) = rows !! (n `posMod` h)

setRow :: Int -> Row -> Grid -> Grid
setRow n row' (Grid h rows) = Grid h $ set (n `posMod` h) row' rows

updateCell :: Int -> Int -> Cell -> Grid -> Grid
updateCell x y c g = setRow y (setCell x c (getRow y g)) g

newGrid :: Int -> Int -> Grid
newGrid w h = Grid h $ replicate h makeRow
  where
    makeRow = Row w $ replicate w Dead

countNeighbors :: Int -> Row -> Row -> Row -> Int
countNeighbors x below above current =
    let neighbors = getCell (x - 1) current : getCell (x + 1) current : beforeCurrentAfter x below ++ beforeCurrentAfter x above
    in getSum . foldMap countAlive $ neighbors
  where
    countAlive Alive = Sum 1
    countAlive Dead = Sum 0

    beforeCurrentAfter n row = [
      getCell (n - 1) row
      , getCell n row
      , getCell (n + 1) row
      ]

gol :: Int -> Row -> Row -> Row -> Cell
gol x below above current = gol' (getCell x current) (countNeighbors x below above current) x
  where
    gol' Alive numNeighbors x
      | numNeighbors < 2 || numNeighbors > 3 = Dead
      | otherwise = Alive
    gol' Dead numNeighbors x
      | numNeighbors == 3 = Alive
      | otherwise = Dead

runGol :: Int -> Int -> Grid -> Row
runGol w y g = let current = getRow y g in runGol' w (getRow (y+1) g) (getRow (y-1) g) current current
  where
    runGol' 0 below above current row = setCell 0 (gol 0 below above current) row
    runGol' w below above current row = setCell w (gol w below above current) (runGol' (w-1) below above current row)

makeGolThread :: MVar Grid -> MVar Grid -> MVar () -> MVar () -> MVar () -> Int -> IO ThreadId
makeGolThread readBuffer writeBuffer threadReturned runThreads pause n = forkIO . forever $ do
  takeMVar runThreads
  doPause <- tryReadMVar pause
  case doPause of
    Just () -> return ()
    Nothing -> do
      newRow <- runGol width n <$> readMVar readBuffer
      updatedWriteBuffer <- setRow n newRow <$> takeMVar writeBuffer
      putMVar writeBuffer updatedWriteBuffer
  putMVar threadReturned ()

showCell :: Cell -> Char
showCell Alive = '#'
showCell Dead = ' '

showGrid :: Grid -> String
showGrid (Grid h rows) = "\n" ++ unlines (showGrid' True rows) ++ "\n"
  where
    showGrid' b [] = []
    showGrid' True (Row w cells:rows) = horizontalBorder w ++ cellsWithBorder cells : showGrid' False rows ++ horizontalBorder w
    showGrid' b (Row w cells:rows) =  cellsWithBorder cells : showGrid' False rows

    horizontalBorder w = [replicate (w+2) '-']
    cellsWithBorder cells = '|' : fmap showCell cells ++ "|"

mainLoop :: MVar Grid -> MVar Grid -> MVar () -> MVar () -> MVar Command -> MVar () -> IO ()
mainLoop readBuffer writeBuffer threadReturned runThreads currentCommand pause = loop
  where
    loop = do
      replicateM_ height $ putMVar runThreads ()
      replicateM_ height $ takeMVar threadReturned
      readBuffer' <- takeMVar readBuffer
      writeBuffer' <- takeMVar writeBuffer
      command <- tryTakeMVar currentCommand
      writeBuffer'' <- case command of
            Just Start -> do
              tryTakeMVar pause
              return writeBuffer'
            Just Stop -> do
              putMVar pause ()
              return writeBuffer'
            Just command -> return $ runCommand command writeBuffer'
            Nothing -> return writeBuffer'
      putStrLn $ showGrid writeBuffer''
      doPause <- tryReadMVar pause
      case doPause of
        Just () -> do
          putMVar readBuffer readBuffer'
          putMVar writeBuffer writeBuffer''
        Nothing -> do
          putMVar readBuffer writeBuffer''
          putMVar writeBuffer readBuffer'
      threadDelay 1000000
      loop

data Position = Position Int Int

data StillType = Block
               | Loaf
               | Beehive
               | Boat

data OscType = Blinker
             | Beacon
             | Toad

data ShipType = Glider

data Command = SetCell Cell Position
             | Still StillType Position
             | Osc OscType Position
             | Ship ShipType Position
             | Start
             | Stop
             | End
             | Help
             | Clear

parseInput :: String -> Maybe Command
parseInput input = parseCommand $ words input
  where
    parsePosition [x,y]
      | all isDigit x && all isDigit y = Just $ Position (read x) (read y)
      | otherwise = Nothing

    parseCommand ("still":"block":rest) = Still Block <$> parsePosition rest
    parseCommand ("still":"loaf":rest) = Still Loaf <$> parsePosition rest
    parseCommand ("still":"beehive":rest) = Still Beehive <$> parsePosition rest
    parseCommand ("still":"boat":rest) = Still Boat <$> parsePosition rest
    parseCommand ("osc":"blinker":rest) = Osc Blinker <$> parsePosition rest
    parseCommand ("osc":"beacon":rest) = Osc Beacon <$> parsePosition rest
    parseCommand ("osc":"toad":rest) = Osc Toad <$> parsePosition rest
    parseCommand ("ship":"glider":rest) = Ship Glider <$> parsePosition rest
    parseCommand ["start"] = Just Start
    parseCommand ["stop"] = Just Stop
    parseCommand ["end"] = Just End
    parseCommand ["help"] = Just Help
    parseCommand ["clear"] = Just Clear
    parseCommand _ = Nothing

draw :: [(Int,Int)] -> Position -> Grid -> Grid
draw [] p = id
draw ((cx,cy):cs) p@(Position x y) = updateCell (cx+x) (cy+y) Alive . draw cs p

block = [
  (0,0), (0,1),
  (1,0), (1,1)
  ]

beehive = [
         (0,1), (0,2),
  (1,0),               (1,3),
  (2,0),               (2,3),
         (3,1), (3,2)
  ]

loaf = [
         (0,1), (0,2),
  (1,0),               (1,3),
         (2,1),        (2,3),
                (3,2)
  ]

boat = [
  (0,0), (0,1),
  (1,0),        (1,2),
         (2,1)
  ]

blinker = [
  (0,1),
  (1,1),
  (2,1)
  ]

beacon = [
  (0,0), (0,1),
  (1,0), (1,1),
                (2,2), (2,3),
                (3,2), (3,3)
  ]

toad = [
         (1,1), (1,2), (1,3),
  (2,0), (2,1), (2,2)
  ]

glider = [
         (0,1),
                (1,2),
  (2,0), (2,1), (2,2)
  ]

runCommand :: Command -> Grid -> Grid
runCommand (Still Block pos) = draw block pos
runCommand (Still Loaf pos) = draw loaf pos
runCommand (Still Beehive pos) = draw beehive pos
runCommand (Still Boat pos) = draw boat pos
runCommand (Osc Blinker pos) = draw blinker pos
runCommand (Osc Beacon pos) = draw beacon pos
runCommand (Osc Toad pos) = draw toad pos
runCommand (Ship Glider pos) = draw glider pos
runCommand Clear = clear
  where
    clear (Grid h (Row w _:_)) = newGrid w h
runCommand _ = id

inputLoop :: MVar Command -> IO ()
inputLoop currentCommand = do
  putStr ":^) "
  hFlush stdout
  input <- getLine
  case parseInput input of
    Just End -> return ()
    Just command -> do
      putMVar currentCommand command
      inputLoop currentCommand
    Nothing -> inputLoop currentCommand

main :: IO ()
main = do
  threadReturned <- newEmptyMVar
  runThreads <- newEmptyMVar
  readBuffer <- newMVar $ newGrid width height
  writeBuffer <- newMVar $ newGrid width height
  pause <- newEmptyMVar
  currentCommand <- newEmptyMVar
  for_ [0..height-1] $ makeGolThread readBuffer writeBuffer threadReturned runThreads pause
  forkIO $ mainLoop readBuffer writeBuffer threadReturned runThreads currentCommand pause
  inputLoop currentCommand
