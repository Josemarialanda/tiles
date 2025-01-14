{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}

module Tiles.Types where

import           Control.Lens        (DefName (..), lensField, lensRules,
                                      makeLensesWith, (&), (.~))
import           Data.Function       (fix, on)
import           Data.Maybe          (isJust, mapMaybe)
import           Data.Text           (Text)
import           Language.Haskell.TH (mkName, nameBase)
import           Polysemy            (makeSem)
import           Polysemy.Error      (Error)
import           Polysemy.State      (State)
import           System.Random
import           Tiles.Utils.Default (Default (..))
import           Tiles.Utils.OneOfN  (OneOf3)

data TileModifier = TileModifier
  { tileModifier'id            :: Text
  , tileModifier'priority      :: Int
  , tileModifier'onMergeEffect :: Maybe (Tile -> Tile)
  , tileModifier'onMoveEffect  :: Maybe (Tile -> Tile)
  }

instance Show TileModifier where
  show :: TileModifier -> String
  show TileModifier{tileModifier'id} = show tileModifier'id

instance Eq TileModifier where
  (==) :: TileModifier -> TileModifier -> Bool
  (==) = (==) `on` tileModifier'id

instance Ord TileModifier where
  compare :: TileModifier -> TileModifier -> Ordering
  compare = compare `on` tileModifier'priority

data TileMeta = TileMeta
  { tileMeta'isMerged :: Bool
  , tileMeta'modifier :: [TileModifier]
  , tileMeta'newSpawn :: Bool
  }

instance Semigroup TileMeta where
  (<>) :: TileMeta -> TileMeta -> TileMeta
  m1 <> m2 = TileMeta
    { tileMeta'isMerged = True
    , tileMeta'modifier = tileMeta'modifier m1 <> tileMeta'modifier m2
    , tileMeta'newSpawn = False
    }

instance Default TileMeta where
  def :: TileMeta
  def = TileMeta {
      tileMeta'isMerged = False
    , tileMeta'modifier = []
    , tileMeta'newSpawn = True
    }

data Tile = Tile {
    tile'value :: Int
  , tile'meta  :: TileMeta
  }

instance Show Tile where
  show :: Tile -> String
  show Tile{tile'value} = show tile'value

instance Eq Tile where
  (==) :: Tile -> Tile -> Bool
  (==) = (==) `on` tile'value

instance Ord Tile where
  compare :: Tile -> Tile -> Ordering
  compare = compare `on` tile'value

instance Semigroup Tile where
  (<>) :: Tile -> Tile -> Tile
  Tile a m1 <> Tile _ m2 = modifier $ Tile (a * 2) meta
    where
      -- | Compose a list of functions
      composeFunctions :: [a -> a] -> a -> a
      composeFunctions = fix (\rec fs -> case fs of
          []     -> id
          (g:gs) -> g . rec gs)


      modifier :: Tile -> Tile
      modifier = onMergeModifierEffect . onMoveModifierEffect

      meta :: TileMeta
      meta = TileMeta
        {
          tileMeta'isMerged = True
        , tileMeta'modifier = tile2OnMoveModifiers
        , tileMeta'newSpawn = False
        }

      onMergeModifierEffect  :: Tile -> Tile
      onMergeModifierEffect =
          composeFunctions
        . mapMaybe tileModifier'onMergeEffect
        $ onMergeModifiers

      onMoveModifierEffect :: Tile -> Tile
      onMoveModifierEffect =
          composeFunctions
        . mapMaybe tileModifier'onMoveEffect
        $ tile1OnMoveModifiers

      onMergeModifiers :: [TileModifier]
      onMergeModifiers = filter (isJust . tileModifier'onMergeEffect) $ tile1Modifiers <> tile2Modifiers

      tile1OnMoveModifiers :: [TileModifier]
      tile1OnMoveModifiers = filter (isJust . tileModifier'onMoveEffect) tile1Modifiers

      tile2OnMoveModifiers :: [TileModifier]
      tile2OnMoveModifiers = filter (isJust . tileModifier'onMoveEffect) tile2Modifiers

      tile1Modifiers :: [TileModifier]
      tile1Modifiers = tileMeta'modifier m1

      tile2Modifiers :: [TileModifier]
      tile2Modifiers = tileMeta'modifier m2

data GridConfig = GridConfig
  { gridConfig'size       :: Int
  , gridConfig'spawnTiles :: Int
  }
  deriving (Show)

data GameSettings = GameSettings
  { gameSettings'gridConfig :: GridConfig
  , gameSettings'undos      :: Maybe Int
  , gameSettings'seed       :: Maybe Int
  }
  deriving (Show)

type Grid = [Row]
type Row = [Maybe Tile]

data GameSnapshot = GameSnapshot
  { gameSnapshot'grid    :: Grid
  , gameSnapshot'dir     :: Maybe Direction
  , gameSnapshot'score   :: Int
  , gameSnapshot'maxTile :: Int
  }
  deriving (Show)

data GameCtx = GameCtx
  { gameCtx'gridConfig :: GridConfig
  , gameCtx'grid       :: Grid
  , gameCtx'history    :: [GameSnapshot]
  , gameCtx'turns      :: Int
  , gameCtx'undosLeft  :: Maybe Int
  , gameCtx'lastError  :: Maybe GameError
  , gameCtx'score      :: Int
  , gameCtx'maxTile    :: Int
  , gameCtx'gen        :: StdGen
  }
  deriving (Show)

data Direction = UP | DOWN | LEFT | RIGHT
  deriving (Show, Eq)

data Action =
    AMove Direction
  | AUndo
  | AQuit
  deriving (Show)

newtype RuntimeError = RuntimeError { unRuntimeError :: OneOf3 StdInError StdOutError GameError }
  deriving newtype (Show)

data GameError =
    NoMoreUndos
  | CannotUndoFromEmptyHistory
  | CannotMoveInDirection Direction
  | GameOver
  deriving (Eq, Show)

data StdInError =
    UnrecognizedInput String
  | EmptyInput
  deriving (Eq, Show)

data StdOutError =
    EmptyOutput
  | CannotWriteOutput
  deriving (Eq, Show)

data Game m a where
  Move      :: Direction -> Game m ()
  Undo      :: Game m ()
  Quit      :: Game m ()
makeSem ''Game

type CoreEff = '[Game, State GameCtx, Error GameError]

makeLensesWith (lensRules & lensField .~ \_ _ name -> [TopName (mkName (nameBase name <> "L"))]) ''GameCtx
