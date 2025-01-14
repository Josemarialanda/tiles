{-# LANGUAGE BlockArguments #-}

module Tiles.Game where

import           Polysemy       (Members, Sem)
import           Polysemy.Input (Input, input)
import           Tiles.Types    (Action (..), Game (..), move, quit, undo)

game :: forall r. Members '[Game, Input Action] r => Sem r ()
game = input >>= \case
  AMove dir -> move dir >> game
  AUndo     -> undo >> game
  AQuit     -> quit
