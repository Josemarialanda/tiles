module Tiles.Action where

import           Control.Lens         ((%~), (&), (+~), (-~), (.~), (?~))
import           Control.Monad.Except (MonadError (..))
import           Control.Monad.State  (MonadState (..), modify)
import           Data.Bifunctor       (first)
import           Data.List            (transpose)
import qualified Data.Map.Strict      as Map
import           Data.Monoid          (Last (..))
import           System.Random        (StdGen)
import           Tiles.Common         (spawnTiles)
import           Tiles.Types          (Bundle, Direction (..), GameCtx (..),
                                       GameError, Grid,
                                       GridConfig (gridConfig'spawnTiles),
                                       GridTile (GridTile))
import           Tiles.Utils.Default  (def)
import           Tiles.Utils.Helpers  (loop)
import qualified Tiles.Validation     as Validation

validate :: Bundle m => m b -> m (Either GameError ()) -> m b
validate action validator = validator >>= either throwError (const action)

move :: forall m b. Bundle m => Direction -> m b -> m b
move direction next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.move direction

    action :: m b
    action = modify (move' direction) >> next

move' :: Direction -> GameCtx -> GameCtx
move' direction ctx@GameCtx{..} = ctx
  { gameCtx'grid = grid'
  , gameCtx'history = gameCtx'history <> [(grid', pure direction)]
  , gameCtx'seedGen = seedGen
  }
  where
    (seedGen, grid') = updateGrid gameCtx'grid

    updateGrid :: Grid -> (StdGen, Grid)
    updateGrid grid
      | grid' == grid = (gameCtx'seedGen, grid)
      | otherwise = spawnTiles gameCtx'seedGen (gridConfig'spawnTiles gameCtx'gridConfig) grid'
      where
        grid' = updateTiles direction grid

    updateTiles :: Direction -> Grid -> Grid
    updateTiles dir = case dir of
      LEFT  -> fmap mergeRow
      RIGHT -> fmap (reverse . mergeRow . reverse)
      UP    -> transpose . updateTiles LEFT . transpose
      DOWN  -> transpose . updateTiles RIGHT . transpose

    mergeRow :: [GridTile] -> [GridTile]
    mergeRow row = row' <> replicate (length row - length row') mempty
      where
        row' :: [GridTile]
        row'  = mergeTiles $ filter (/= mempty) row

        mergeTiles :: [GridTile] -> [GridTile]
        mergeTiles (x:y:xs)
          | x == y = x <> y : mergeTiles xs
          | otherwise = x : mergeTiles (y:xs)
        mergeTiles x = x
