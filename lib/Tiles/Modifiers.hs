module Tiles.Modifiers where

import           Data.Maybe         (maybeToList)

import           Polysemy           (Member, Sem)
import           Tiles.Types        (Grid, Tile (..), TileMeta (..),
                                     TileModifier (..))
import           Tiles.Utils.Random (Random, pickRandom)

addModifiers :: forall r. Grid -> Member Random r => Sem r Grid
addModifiers grid = do
  modifiers <- getModifiers (length grid * length grid)
  pure $ fmap (uncurry addModifier) <$> zipMatrix modifiers grid
  where
    getModifiers :: Int -> Member Random r => Sem r [Maybe TileModifier]
    getModifiers = go []
      where
        go :: [Maybe TileModifier] -> Int -> Member Random r => Sem r [Maybe TileModifier]
        go acc 0 = pure acc
        go acc n = do
          modifier <- pickRandom tileModifiers
          go (modifier : acc) (n - 1)

    addModifier :: Maybe TileModifier -> Maybe Tile -> Maybe Tile
    addModifier modifier (Just tile)
      | tileMeta'newSpawn $ tile'meta tile =
        Just $ tile
          { tile'meta = (tile'meta tile)
            { tileMeta'modifier = maybeToList modifier
            , tileMeta'newSpawn = False
            }
          }
      | otherwise = Just tile
    addModifier _ Nothing = Nothing

    zipMatrix :: [a] -> [[b]] -> [[(a, b)]]
    zipMatrix _ [] = []
    zipMatrix xs (row:rows) =
      let (rowElems, rest) = splitAt (length row) xs
      in zip rowElems row : zipMatrix rest rows

-- Tile modifier definitions

tileModifiers :: [Maybe TileModifier]
tileModifiers =
     replicate 100 Nothing
  <> replicate 30 (Just modifierOnMergeMultiplier2)
  <> replicate 15 (Just modifierOnMergeMultiplier4)
  <> replicate 10 (Just modifierOnMergeDivider2)
  <> replicate 5 (Just modifierOnMergeDivider4)

-- | Multiplies the value of the tile by 2 when merged
modifierOnMergeMultiplier2 :: TileModifier
modifierOnMergeMultiplier2 = TileModifier
  { tileModifier'id = "om-mult-2"
  , tileModifier'priority = 1
  , tileModifier'onMergeEffect = Just $ \tile -> tile { tile'value = tile'value tile * 2 }
  , tileModifier'onMoveEffect = Nothing
  }

-- | Multiplies the value of the tile by 4 when merged
modifierOnMergeMultiplier4 :: TileModifier
modifierOnMergeMultiplier4 = TileModifier
  { tileModifier'id = "om-mult-4"
  , tileModifier'priority = 1
  , tileModifier'onMergeEffect = Just $ \tile -> tile { tile'value = tile'value tile * 4 }
  , tileModifier'onMoveEffect = Nothing
  }

-- | Divides the value of the tile by 2 when merged.
--   If the value is 0 or 1, it is set to 2
modifierOnMergeDivider2 :: TileModifier
modifierOnMergeDivider2 = TileModifier
  { tileModifier'id = "om-div-2"
  , tileModifier'priority = 1
  , tileModifier'onMergeEffect = Just $ \tile -> tile
    { tile'value = case div (tile'value tile) 2 of
        0 -> 2
        1 -> 2
        x -> x
    }
  , tileModifier'onMoveEffect = Nothing
  }

-- | Divides the value of the tile by 4 when merged.
--   If the value is 0 or 1, it is set to 2
modifierOnMergeDivider4 :: TileModifier
modifierOnMergeDivider4 = TileModifier
  { tileModifier'id = "om-div-4"
  , tileModifier'priority = 1
  , tileModifier'onMergeEffect = Just $ \tile -> tile
    { tile'value = case div (tile'value tile) 4 of
        0 -> 2
        1 -> 2
        x -> x
    }
  , tileModifier'onMoveEffect = Nothing
  }
