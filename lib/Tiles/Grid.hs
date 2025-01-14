module Tiles.Grid where

import           Data.List            (sortOn, transpose)
import           Data.Maybe           (listToMaybe)
import           Polysemy             (Member, Sem)
import           Text.Printf          (printf)
import           Tiles.Tiles          (spawnTiles)
import           Tiles.Types          (Grid, GridConfig (..), Tile (..),
                                       TileMeta (..), TileModifier (..))
import           Tiles.Utils.Random   (Random)
import           Tiles.Utils.Terminal (Color (..), colorString)

createRandomGrid :: forall r. Member Random r => GridConfig -> Sem r Grid
createRandomGrid GridConfig{..} =
    spawnTiles gridConfig'spawnTiles
  $ emptyGrid gridConfig'size

emptyGrid :: Int -> Grid
emptyGrid size = replicate size $ replicate size mempty

maxGridValue :: Grid -> Int
maxGridValue = maximum . fmap (maximum . fmap (maybe 0 tile'value))

-- | Pretty print a Grid
printGrid :: Grid -> IO ()
printGrid grid = mapM_ putStrLn paddedRows
  where
    stringMatrix :: [[String]]
    stringMatrix = fmap (fmap showTile) grid

    showTile :: Maybe Tile -> String
    showTile (Just (Tile value (TileMeta{tileMeta'modifier}))) = case listToMaybe (sortOn tileModifier'priority tileMeta'modifier) of
      Just (TileModifier{tileModifier'id = "om-mult-2"}) -> colorString Green $ printf "%3d" value
      Just (TileModifier{tileModifier'id = "om-mult-4"}) -> colorString Yellow $ printf "%3d" value
      Just (TileModifier{tileModifier'id = "om-div-2"}) -> colorString Red $ printf "%3d" value
      Just (TileModifier{tileModifier'id = "om-div-4"}) -> colorString Blue $ printf "%3d" value
      _ -> colorString White $ printf "%3d" value
    showTile Nothing = colorString Gray $ printf "%s" ("  .  " :: String)

    colWidths :: [Int]
    colWidths = fmap (maximum . fmap length) (transpose stringMatrix)

    paddedRows :: [String]
    paddedRows = fmap (padRow colWidths) stringMatrix

    padRow :: [Int] -> [String] -> String
    padRow widths row = unwords $ zipWith padRight widths row

    padRight :: Int -> String -> String
    padRight n s = s <> replicate (n - length s) ' '
