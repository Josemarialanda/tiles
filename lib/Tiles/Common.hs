module Tiles.Common where

import           Data.Bifunctor      (Bifunctor (first))
import qualified Data.Map            as Map
import           Data.Monoid         (Last (..))
import           System.Random       (Random (randomR), RandomGen, mkStdGen,
                                      randomRIO)
import           Text.Printf         (printf)
import           Tiles.Types         (Grid, GridTile (GridTile), Tile (Tile))
import           Tiles.Utils.Helpers (loop)

-- shuffle using the Fisher Yates algorithm
shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen l = toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems :: (Map.Map k a, b) -> ([a], b)
    toElems (x, y) = (Map.elems x, y)

    numerate :: [a] -> [(Int, a)]
    numerate = zip [1 ..]

    initial :: a -> g -> (Map.Map Int a, g)
    initial x gen = (Map.singleton 0 x, gen)

    fisherYatesStep :: forall g a. RandomGen g => (Map.Map Int a, g) -> (Int, a) -> (Map.Map Int a, g)
    fisherYatesStep (m, gen) (i, x) =
      ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
      where
        (j :: Int, gen' :: g) = randomR (0, i) gen

-- | Pick n random elements after shuffling with a given seed
pickNRandom :: RandomGen g => g -> Int -> [a] -> ([a], g)
pickNRandom seed n = first (take n) . shuffle seed

-- | Pick random element after shuffling with a given seed
pickRandom :: RandomGen g => g -> [a] -> (a, g)
pickRandom seed xs = first head $ pickNRandom seed 1 xs

-- | Pick a random spawn tile value (2 (90%) or 4 (10%)) with a given seed
pickSpawnTileValue :: RandomGen g => g -> (Int, g)
pickSpawnTileValue seed = pickRandom seed [2, 2, 2, 2, 2, 2, 2, 2, 2, 4]

-- | Pretty print a Grid
printGrid :: Grid -> IO ()
printGrid = mapM_ (putStrLn . showRow)
  where
    showRow :: [GridTile] -> [Char]
    showRow = concatMap (printf "%5d" . f)

    f :: GridTile -> Int
    f (GridTile (Last (Just (Tile value _)))) = value
    f (GridTile (Last Nothing))               = 0

-- | Spawn n tiles on the grid with a given seed
spawnTiles :: RandomGen g => g -> Int -> Grid -> (g, Grid)
spawnTiles = go
  where
    go seed 0 grid = (seed, grid)
    go seed n grid =
      let (seed', grid') = spawnTile seed grid
      in go seed' (n - 1) grid'

    spawnTile :: RandomGen g => g -> Grid -> (g, Grid)
    spawnTile seed grid = (seed'', grid')
      where
        candidates = getEmptyTiles grid
        (pick, seed') = pickRandom seed candidates
        (value, seed'') = pickSpawnTileValue seed'
        grid' = setTile grid pick value

-- | Set a tile on the grid
setTile :: Grid -> (Int, Int) -> Int -> Grid
setTile grid (row, col) value = pre <> [mid] <> post
  where
    pre  = take row grid
    mid  = take col (grid!!row) <> ([GridTile $ pure $ Tile value ()] <> drop (col + 1) (grid!!row))
    post = drop (row + 1) grid

-- | Get empty tiles on the grid
getEmptyTiles :: Grid -> [(Int, Int)]
getEmptyTiles grid = filter (\(row, col) -> (grid!!row)!!col == mempty) coordinates
  where
    singleRow n = zip (replicate (length grid) n) [0..length grid - 1]
    coordinates = concatMap singleRow [0..length grid - 1]
