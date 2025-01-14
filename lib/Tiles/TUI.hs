{-# LANGUAGE BlockArguments #-}

module Tiles.TUI where

import           Control.Monad        (void, when)
import           Data.Char            (toLower)
import           Data.Maybe           (fromMaybe)
import           Polysemy             (Embed, Members, Sem, embed, embedToFinal,
                                       interpret, runFinal)
import           Polysemy.Error       (Error, catch, errorToIOFinal, throw)
import           Polysemy.Input       (runInputSem)
import           Polysemy.State       (State, evalState, get)
import           System.Exit          (exitSuccess)
import           System.IO            (BufferMode (NoBuffering), hReady,
                                       hSetBuffering, hSetEcho, stdin)
import           System.Random        (mkStdGen)
import qualified Tiles.Action         as Action
import           Tiles.Game           (game)
import           Tiles.Grid           (createRandomGrid, maxGridValue,
                                       printGrid)
import           Tiles.Types          (Action (..),
                                       Direction (DOWN, LEFT, RIGHT, UP),
                                       Game (..), GameCtx (..), GameError (..),
                                       GameSettings (..), GameSnapshot (..),
                                       GridConfig (..),
                                       RuntimeError (RuntimeError))
import           Tiles.Utils.OneOfN   (OneOf3 (ThreeOf3))
import           Tiles.Utils.Random   (Random, getGen, randomIOWithGen,
                                       randomToIO, setGen)
import           Tiles.Utils.Terminal (Color (..), clearScreen, colorPrint,
                                       newline)

run :: IO ()
run = do
  gameCtx@GameCtx{gameCtx'gen} <-
      runFinal
    . embedToFinal @IO
    . randomToIO
    $ setupGame

  void $ runFinal
    . embedToFinal @IO
    . errorToIOFinal @RuntimeError
    . evalState gameCtx
    . runInputSem getNextAction
    . randomIOWithGen gameCtx'gen
    $ runGame game

  where
    getNextAction :: forall r. Members '[Embed IO, Error RuntimeError] r => Sem r Action
    getNextAction = embed getKey >>= \case
      "\ESC[A" -> pure $ AMove UP
      "\ESC[B" -> pure $ AMove DOWN
      "\ESC[C" -> pure $ AMove RIGHT
      "\ESC[D" -> pure $ AMove LEFT
      "u"      -> pure AUndo
      "q"      -> pure AQuit
      _        -> getNextAction
      where
        getKey :: IO String
        getKey = reverse <$> getKey' ""
          where
            getKey' chars = do
              char <- getChar
              more <- hReady stdin
              (if more then getKey' else return) (char:chars)

runGame :: forall r. Members '[Error RuntimeError, State GameCtx, Embed IO, Random] r => Sem (Game ': r) () -> Sem r ()
runGame = flip (catch @RuntimeError) errorHandler . go
  where
    go :: Sem (Game : r) () -> Sem r ()
    go = interpret \case
      Move dir  -> do
        Action.move dir
        get >>= embed . updateScreen
      Undo      -> do
        Action.undo
        get >>= embed . updateScreen
      Quit      -> do
        embed newline
        embed $ putStrLn "Goodbye! 👋"
        embed exitSuccess

    errorHandler :: RuntimeError -> Sem r ()
    errorHandler (RuntimeError (ThreeOf3 gameError)) = case gameError of
      NoMoreUndos -> embed $ putStrLn "No more undos left. 😢"
      GameOver -> do
        embed $ putStrLn "Game over. 🤮"
        embed $ putStrLn "Would you like to play again? (y/n) 😛"
        inp <- embed getChar
        if toLower inp == 'y'
          then embed run
          else embed exitSuccess
      CannotMoveInDirection direction -> embed $ putStrLn $ "Cannot move in direction " <> show direction
      CannotUndoFromEmptyHistory -> embed $ putStrLn "Cannot undo from empty history."
    errorHandler stdInOutError = throw stdInOutError

updateScreen :: GameCtx -> IO ()
updateScreen GameCtx{..} = do
  clearScreen
  printWinner
  printGameDetails
  printGrid gameCtx'grid
  where
    printWinner :: IO ()
    printWinner = do
      when (gameCtx'maxTile >= 2048) $ do
        putStrLn "You win! You've reached 2048! 🎉"

    printGameDetails :: IO ()
    printGameDetails = do
      putStrLn $ "🐵 Grid Size: " <> show (gridConfig'size gameCtx'gridConfig)
      putStrLn $ "🐆 Spawn Tiles: " <> show (gridConfig'spawnTiles gameCtx'gridConfig)
      putStrLn $ "🐷 Turns: " <> show gameCtx'turns
      putStrLn $ "🦔 Undos left: " <> show (fromMaybe maxBound gameCtx'undosLeft )
      putStrLn $ "🐼 Max tile: " <> show gameCtx'maxTile
      putStrLn $ "🐀 Score: " <> show gameCtx'score
      newline
      colorPrint Green "Tiles with X2 multiplier are colored in green."
      colorPrint Yellow "Tiles with X4 multiplier are colored in yellow."
      colorPrint Red "Tiles with ÷2 divider are colored in red."
      colorPrint Blue "Tiles with ÷4 divider are colored in Blue."
      newline

setupGame :: forall r. Members '[Embed IO, Random] r => Sem r GameCtx
setupGame  = do
  embed $ do
    hSetBuffering stdin NoBuffering
    clearScreen
    putStrLn "🔥🔥🔥 Welcome to 2048! sizzlin' hot edition! 🔥🔥🔥"
    putStrLn ""
    putStrLn "Controls:"
    putStrLn "↑ - Move up"
    putStrLn "↓ - Move down"
    putStrLn "→ - Move right"
    putStrLn "← - Move left"
    putStrLn "u - Undo"
    putStrLn "q - Quit"
  gameSettings <- getGameSettings
  gameCtx <- createGameCtx gameSettings
  embed $ updateScreen gameCtx
  pure gameCtx
  where
    getGameSettings :: Sem r GameSettings
    getGameSettings = embed $ do
      putStrLn "What size grid would you like to play on? (options: 4, 5, 6)."
      gridSize <- getGridSize
      putStrLn "How many spawn tiles would you like to start with? (options: 1, 2, 3)."
      spawnTiles <- getSpawnTiles
      putStrLn "How many undo steps would you like to have?"
      undoSteps <- getUndoSteps
      putStrLn "Seed for random number generator? (type any number or leave empty for random seed)"
      seed <- getSeed
      hSetEcho stdin False
      pure $ GameSettings (GridConfig gridSize spawnTiles) undoSteps seed
      where
        getGridSize :: IO Int
        getGridSize = do
          gridSizeInput <- getLine
          case reads gridSizeInput of
            [(n, _)] -> if n `elem` [4,5,6]
              then pure n
              else do
                putStrLn "Invalid input. Please enter 4, 5, or 6."
                getGridSize
            _        -> do
              putStrLn "Invalid input. Please enter 4, 5, or 6."
              getGridSize

        getSpawnTiles :: IO Int
        getSpawnTiles = do
          spawnTilesInput <- getLine
          case reads spawnTilesInput of
            [(n, _)] -> if n `elem` [1,2,3]
              then pure n
              else do
                putStrLn "Invalid input. Please enter 1, 2, or 3."
                getSpawnTiles
            _        -> do
              putStrLn "Invalid input. Please enter 1, 2, or 3."
              getSpawnTiles

        getUndoSteps :: IO (Maybe Int)
        getUndoSteps = do
          undoStepsInput <- getLine
          case reads undoStepsInput of
            [(n, _)] -> if n >= 0
              then pure $ Just n
              else do
                putStrLn "Invalid input. Please enter a non-negative number."
                getUndoSteps
            _        -> do
              putStrLn "Invalid input. Please enter a non-negative number."
              getUndoSteps

        getSeed :: IO (Maybe Int)
        getSeed = do
          seedInput <- getLine
          if seedInput == ""
            then pure Nothing
            else case reads seedInput of
              [(n, _)] -> pure $ Just n
              _        -> do
                putStrLn "Invalid input. Please enter a number."
                getSeed

    createGameCtx :: GameSettings -> Sem r GameCtx
    createGameCtx GameSettings{..} = do
      grid <- createRandomGrid gameSettings'gridConfig
      maybe (pure ()) (setGen . mkStdGen) gameSettings'seed
      gen  <- getGen
      pure $ GameCtx
        { gameCtx'gridConfig = gameSettings'gridConfig
        , gameCtx'grid       = grid
        , gameCtx'history    =
            [ GameSnapshot
                { gameSnapshot'grid    = grid
                , gameSnapshot'dir     = Nothing
                , gameSnapshot'score   = 0
                , gameSnapshot'maxTile = maxGridValue grid
                }
            ]
        , gameCtx'turns      = 0
        , gameCtx'undosLeft  = gameSettings'undos
        , gameCtx'lastError  = Nothing
        , gameCtx'score      = 0
        , gameCtx'maxTile    = maxGridValue grid
        , gameCtx'gen        = gen
        }
