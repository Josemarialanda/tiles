module Tiles.Tiles where

import           Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)
import           Data.List            (transpose)

import           Tiles.Types          (Direction (..), Grid, Row, Tile (..),
                                       TileMeta (..))
import           Tiles.Utils.Default  (Default (def))


import           Data.Bifunctor       (Bifunctor (bimap))
import           Data.Semigroup       (Max (..), Sum (..))
import           Polysemy             (Member, Sem)
import           Tiles.Utils.Helpers  (fmap3)
import           Tiles.Utils.Random   (Random, pickRandom)

-- | Spawn n tiles on the grid with a given generator
spawnTiles :: forall r. Member Random r => Int -> Grid -> Sem r Grid
spawnTiles = go
  where
    go :: Member Random r => Int -> Grid -> Sem r Grid
    go 0 grid = pure grid
    go n grid
      | null (getEmptyTiles grid) = pure grid
      | otherwise = spawnTile grid >>= go (n - 1)

    spawnTile :: Member Random r => Grid -> Sem r Grid
    spawnTile grid = do
      tile <- pickRandom $ getEmptyTiles grid
      value <- pickRandom [2, 2, 2, 2, 2, 2, 2, 2, 2, 4]
      pure $ setTile grid tile value

    getEmptyTiles :: Grid -> [(Int, Int)]
    getEmptyTiles grid = filter (\(row, col) -> (grid!!row)!!col == mempty) coordinates
      where
        singleRow n = zip (replicate (length grid) n) [0..length grid - 1]
        coordinates = concatMap singleRow [0..length grid - 1]

    setTile :: Grid -> (Int, Int) -> Int -> Grid
    setTile grid (row, col) value = pre <> [mid] <> post
      where
        pre  = take row grid
        mid  = take col (grid!!row) <> ([pure $ Tile value def] <> drop (col + 1) (grid!!row))
        post = drop (row + 1) grid

-- | Returns the new grid with shifted tiles and the score and
--   max tile value produced by the shift
shiftTiles :: Direction -> [[Maybe Tile]] -> (Grid, (Int, Int))
shiftTiles dir = bimap setNewSpawn (bimap getSum getMax) . runWriter . go dir
  where
    go :: Direction -> [Row] -> Writer (Sum Int, Max Int) Grid
    go dir' grid = do
      tell (0, 0)
      case dir' of
        LEFT  -> traverse merge grid
        RIGHT -> traverse (fmap reverse . merge . reverse) grid
        UP    -> transpose <$> go LEFT (transpose grid)
        DOWN  -> transpose <$> go RIGHT (transpose grid)

    merge :: Row -> Writer (Sum Int, Max Int) Row
    merge row = do
      row' <- mergeTiles (filter (/= mempty) row)
      pure $ row' <> replicate (length row - length row') mempty

    mergeTiles :: Row -> Writer (Sum Int, Max Int) Row
    mergeTiles (x:y:xs)
      | x == y = do
        let merged = x <> y
            value = maybe 0 tile'value merged
        tell (Sum value, Max value)
        (merged :) <$> mergeTiles xs
      | otherwise = (x :) <$> mergeTiles (y:xs)
    mergeTiles x = pure x

    setNewSpawn :: Grid -> Grid
    setNewSpawn = fmap3 $
      \tile -> tile { tile'meta = (tile'meta tile) { tileMeta'newSpawn = False } }
