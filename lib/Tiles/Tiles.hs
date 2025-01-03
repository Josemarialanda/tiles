module Tiles.Tiles where

import           Control.Monad.Except       (ExceptT, MonadError, runExceptT)
import           Control.Monad.Free         (Free, MonadFree, iterM, liftF)
import           Control.Monad.State        (MonadState, StateT (..))
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Char                  (toLower)
import           Data.Functor               ((<&>))
import           Data.List                  (group, intersperse, subsequences,
                                             transpose)
import           Data.List.Split            (chunksOf)
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           System.Random              (Random (randomR), RandomGen,
                                             StdGen, mkStdGen)
import           Text.Printf                (printf)
import qualified Tiles.Action               as Action
import           Tiles.Common               (pickNRandom, pickRandom,
                                             pickSpawnTileValue, printGrid,
                                             spawnTiles)
import           Tiles.Types                (Direction, GameCtx (..), GameError,
                                             GameF (..), Grid, GridConfig (..),
                                             GridTile (GridTile), Tile (Tile))

type Game = Free GameF

move :: MonadFree GameF m => Direction -> m ()
move a = liftF (Move a ())

newtype Interpreter a = Interpreter
  { runInterpreter :: StateT GameCtx (ExceptT GameError IO) a }
  deriving (Functor, Applicative, Monad, MonadError GameError, MonadState GameCtx)

game :: GameF (Interpreter a) -> Interpreter a
game = \case
  Move            direction next -> Action.move direction next

interpret :: Game a -> Interpreter a
interpret = iterM game

run :: Int -> GridConfig -> Game a -> IO (Either GameError (a, GameCtx))
run seed gridConfig = runExceptT . flip runStateT (mkGameFromSeed seed gridConfig) . runInterpreter . interpret

mkGameFromSeed :: Int -> GridConfig -> GameCtx
mkGameFromSeed seed gridConfig = GameCtx
  { gameCtx'gridConfig = gridConfig
  , gameCtx'seed = seed
  , gameCtx'seedGen = seedGen'
  , gameCtx'grid = grid
  , gameCtx'history = [(grid, mempty)]
  }
  where
    (seedGen', grid) = mkGridFromSeed (mkStdGen seed) gridConfig

mkEmptyGrid :: Int -> Grid
mkEmptyGrid n = replicate n [mempty, mempty, mempty, mempty]

mkGridFromSeed :: RandomGen g => g -> GridConfig -> (g, Grid)
mkGridFromSeed seed GridConfig{..} = spawnTiles seed 2 $ mkEmptyGrid gridConfig'size
