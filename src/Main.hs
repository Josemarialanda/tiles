module Main where

import           Data.Monoid   (Last (getLast))
import           System.Random (randomIO)
import           Tiles.Common  (printGrid)
import           Tiles.Tiles   (Game, move, run)
import           Tiles.Types   (Direction (DOWN, RIGHT),
                                GameCtx (GameCtx, gameCtx'grid, gameCtx'gridConfig, gameCtx'history, gameCtx'seed, gameCtx'seedGen),
                                Grid, GridConfig (GridConfig))

main :: IO ()
main = runGame

exampleGame :: Game ()
exampleGame = do
  move DOWN
  move RIGHT
  move RIGHT
  move RIGHT
  move RIGHT

runGame :: IO ()
runGame = do
  let randomSeed = 0
  gameResult <- run randomSeed (GridConfig 4 1) exampleGame
  case gameResult of
    Left err              -> error $ show err
    Right (_,GameCtx{..}) -> mapM_ printHist gameCtx'history
  where
    printHist :: (Grid, Last Direction) -> IO ()
    printHist (grid, direction) = do
      printGrid grid
      putStrLn ("Direction: " <> show (getLast direction))
      putStrLn "-----------------"
