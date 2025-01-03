{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}

module Tiles.Types where

import           Control.Lens         (DefName (..), lensField, lensRules,
                                       makeLensesWith, (&), (.~))
import           Control.Monad.Except (MonadError)
import           Control.Monad.State  (MonadState)
import           Data.Function        (on)
import           Data.Map             (Map)
import           Data.Monoid          (Last (..))
import           Language.Haskell.TH  (mkName, nameBase)
import           System.Random        (StdGen)
import           Tiles.Utils.Default  (Default (..))

type Bundle m = (MonadState GameCtx m, MonadError GameError m)

data GameF next =
    Move Direction next
  deriving (Functor)

data Direction = UP | DOWN | LEFT | RIGHT
  deriving (Show, Eq)

data Tile = Tile
  { tile'value :: Int
  , tile'extra :: ()
  }
  deriving (Eq, Show)

instance Semigroup Tile where
  (<>) :: Tile -> Tile -> Tile
  Tile a _ <> Tile b _ = Tile (a * b) ()

data GridConfig = GridConfig
  { gridConfig'size       :: Int
  , gridConfig'spawnTiles :: Int
  }
  deriving (Show)

newtype GridTile = GridTile { unGridTile :: Last Tile }
  deriving (Eq, Show)

instance Semigroup GridTile where
  (<>) :: GridTile -> GridTile -> GridTile
  GridTile (Last fa) <> GridTile (Last fb) = GridTile $ Last $ fa <> fb

instance Monoid GridTile where
  mempty :: GridTile
  mempty = GridTile $ Last mempty

instance Default GridTile where
  def :: GridTile
  def = mempty

type Grid = [[GridTile]]

data GameCtx = GameCtx
  { gameCtx'gridConfig :: GridConfig
  , gameCtx'seed       :: Int
  , gameCtx'seedGen    :: StdGen
  , gameCtx'grid       :: Grid
  , gameCtx'history    :: [(Grid, Last Direction)]
  }
  deriving (Show)

data GameError
  = GenericError
  deriving (Eq, Show)

makeLensesWith (lensRules & lensField .~ \_ _ name -> [TopName (mkName (nameBase name <> "L"))]) ''GameCtx
