{-# LANGUAGE BlockArguments #-}
module Tiles.Action where

import           Polysemy           (Members, Sem)
import           Polysemy.Error     (Error)
import           Polysemy.State     (State, get, modify, put)
import           Tiles.Modifiers    (addModifiers)
import           Tiles.Tiles        (shiftTiles, spawnTiles)
import           Tiles.Types        (Direction (..), GameCtx (..), GameError,
                                     GameSnapshot (..), GridConfig (..),
                                     RuntimeError (..))
import           Tiles.Utils.Random (Random, getGen)
import qualified Tiles.Validation   as Validation

validate :: forall r. Members '[Error RuntimeError, State GameCtx] r => Sem r () -> Sem r (Either GameError ()) -> Sem r ()
validate action validator = validator >>= either errorHandler (const action)
  where
    errorHandler :: GameError -> Sem r ()
    errorHandler gameError = do
      modify \gameCtx -> gameCtx { gameCtx'lastError = Just gameError }

move :: forall r. Direction -> Members '[Error RuntimeError, State GameCtx, Random] r => Sem r ()
move direction = validate action validator
  where
    validator :: Sem r (Either GameError ())
    validator = Validation.move direction

    action :: Sem r ()
    action = get >>= move' direction >>= put

undo :: forall r. Members '[Error RuntimeError, State GameCtx] r => Sem r ()
undo = validate action validator
  where
    validator :: Sem r (Either GameError ())
    validator = Validation.undo

    action :: Sem r ()
    action = modify undo'

move' :: forall r. Direction -> GameCtx -> Members '[Error RuntimeError, State GameCtx, Random] r => Sem r GameCtx
move' direction ctx@GameCtx{..} = do
  grid' <- spawnTiles (gridConfig'spawnTiles gameCtx'gridConfig) grid
  grid'' <- addModifiers grid'
  gen <- getGen
  pure $ ctx
    { gameCtx'grid = grid''
    , gameCtx'history = gameCtx'history <>
        [ GameSnapshot
          { gameSnapshot'grid    = grid''
          , gameSnapshot'dir     = pure direction
          , gameSnapshot'score   = score
          , gameSnapshot'maxTile = maxTile
          }
        ]
    , gameCtx'turns = gameCtx'turns + 1
    , gameCtx'lastError = Nothing
    , gameCtx'score = score
    , gameCtx'maxTile = maxTile
    , gameCtx'gen     = gen
    }
  where
    (grid, (shiftScore, shiftMaxTile)) = shiftTiles direction gameCtx'grid
    score = gameCtx'score + shiftScore
    maxTile = max gameCtx'maxTile shiftMaxTile

undo' :: GameCtx -> GameCtx
undo' gameCtx@GameCtx{..} = gameCtx
  { gameCtx'grid      = gameSnapshot'grid $ last history
  , gameCtx'history   = history
  , gameCtx'undosLeft = (-) <$> gameCtx'undosLeft <*> pure 1
  , gameCtx'score     = gameSnapshot'score $ last history
  , gameCtx'maxTile   = gameSnapshot'maxTile $ last history
  }
  where
    history = init gameCtx'history
