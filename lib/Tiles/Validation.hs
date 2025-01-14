module Tiles.Validation where

import           Polysemy       (Members, Sem)
import           Polysemy.State (State, get)
import           Tiles.Tiles    (shiftTiles)
import           Tiles.Types    (Direction (..), GameCtx (..), GameError (..),
                                 Grid)


success :: Sem r (Either a ())
success = pure $ Right ()

move :: forall r. Direction -> Members '[State GameCtx] r => Sem r (Either GameError ())
move direction = get >>= canMove
  where
    canMove :: GameCtx -> Sem r (Either GameError ())
    canMove gameCtx@GameCtx{gameCtx'grid}
      | isgameOver gameCtx = pure $ Left GameOver
      | not $ canMoveInDir direction gameCtx'grid = pure $ Left $ CannotMoveInDirection direction
      | otherwise = success

    isgameOver :: GameCtx -> Bool
    isgameOver  GameCtx{gameCtx'grid, gameCtx'undosLeft} =
         not (canMoveInDir UP gameCtx'grid)
      && not (canMoveInDir DOWN gameCtx'grid)
      && not (canMoveInDir LEFT gameCtx'grid)
      && not (canMoveInDir RIGHT gameCtx'grid)
      && gameCtx'undosLeft == Just 0

    canMoveInDir :: Direction -> Grid -> Bool
    canMoveInDir dir grid = fst (shiftTiles dir grid) /= grid

undo :: forall r. Members '[State GameCtx] r => Sem r (Either GameError ())
undo = get >>= canUndo
  where
    canUndo :: GameCtx -> Sem r (Either GameError ())
    canUndo GameCtx{gameCtx'history, gameCtx'undosLeft}
      | not $ maybe True (>= 1) gameCtx'undosLeft = pure $ Left NoMoreUndos
      | length gameCtx'history == 1 = pure $ Left CannotUndoFromEmptyHistory
      | otherwise = success
