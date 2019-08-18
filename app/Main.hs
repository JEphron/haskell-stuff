module Main where

import qualified System.Console.ANSI           as ANSI
import qualified Data.List                     as List

main :: IO ()
main =
  let size = 8
      grid = initGrid size
  in  do
        drawGrid grid
        putStrLn "\n"

drawCell :: Cell -> IO ()
drawCell (Cell color occupied) =
  let char = if occupied then '●' else ' ' in withColor color (putChar char)

drawGrid :: Grid -> IO ()
drawGrid (Grid x) = mapM_ drawRow x

drawRow :: [Cell] -> IO ()
drawRow x = do
  mapM_ drawCell x
  putChar '\n'

boardColorToANSI :: Color -> ANSI.Color
boardColorToANSI Red   = ANSI.Green
boardColorToANSI Black = ANSI.Black

cellColorToANSI :: Color -> ANSI.Color
cellColorToANSI Red   = ANSI.Red
cellColorToANSI Black = ANSI.Yellow

withColor :: Color -> IO () -> IO ()
withColor color io = do
  setForegroundColor (cellColorToANSI color)
  setBackgroundColor (boardColorToANSI color)
  io
  clearColor

clearColor :: IO ()
clearColor = ANSI.setSGR [ANSI.Reset]

setForegroundColor :: ANSI.Color -> IO ()
setForegroundColor color =
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid color]

setBackgroundColor :: ANSI.Color -> IO ()
setBackgroundColor color =
  ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid color]

initGrid :: Int -> Grid
initGrid size =
  let range = [0 .. (size - 1)]
      initCell y x =
          let
            isBlack    = odd (x + y)
            color      = if isBlack then Black else Red
            isOccupied = (isBlack && y > (size - 4)) || (not isBlack && (y < 3))
          in
            Cell color isOccupied

      initRow y = map (initCell y) range
  in  Grid $ map initRow range

newtype Grid = Grid [[Cell]]

data Color = Red | Black

data Cell = Cell Color Bool

