module Tiles.Validation where

import           Control.Monad.Except (MonadError (..))
import           Control.Monad.State  (MonadState (..), modify)
import qualified Data.Map             as Map
import           Data.Maybe           (isJust)
import           Tiles.Types          (Bundle, Direction, GameCtx, GameError)

success ::  Bundle m => m (Either GameError ())
success = return $ Right ()

move :: forall m. Bundle m => Direction -> m (Either GameError ())
move direction = get >>= canMove
  where
    canMove :: GameCtx -> m (Either GameError ())
    canMove ctx = success
