{-# LANGUAGE BlockArguments #-}

module Tiles.Utils.Random where

import           Polysemy       (Embed, Member, Sem, makeSem, reinterpret)
import           Polysemy.State (get, gets, put, runState)
import qualified System.Random  as R

data Random m a where
  Random      :: R.Random x => Random m x
  RandomR     :: R.Random x => (x, x) -> Random m x
  Randoms     :: R.Random x => Random m [x]
  RandomRs    :: R.Random x => (x, x) -> Random m [x]
  PickRandom  :: [x] -> Random m x
  PickRandoms :: Int -> [x] -> Random m [x]
  GetGen      :: Random m R.StdGen
  SetGen      :: R.StdGen -> Random m ()
makeSem ''Random

randomToIO :: Member (Embed IO) r => Sem (Random ': r) a -> Sem r a
randomToIO m = do
  q <- R.newStdGen
  snd <$> runRandom q m

randomToIOWithSeed :: Member (Embed IO) r => Int -> Sem (Random ': r) a -> Sem r a
randomToIOWithSeed seed m = snd <$> runRandom (R.mkStdGen seed) m

randomIOWithGen :: Member (Embed IO) r => R.StdGen -> Sem (Random ': r) a -> Sem r a
randomIOWithGen q = fmap snd . runRandom q

runRandom :: forall r a. R.StdGen -> Sem (Random ': r) a -> Sem r (R.StdGen, a)
runRandom q = runState q . reinterpret \case
  Random -> do
    (a, q') <- gets @R.StdGen R.random
    put q'
    pure a
  RandomR r -> do
    (a, q') <- gets @R.StdGen $ R.randomR r
    put q'
    pure a
  Randoms -> gets @R.StdGen $ R.randoms
  RandomRs r -> gets @R.StdGen $ R.randomRs r
  PickRandom xs -> do
    (a, q') <- gets @R.StdGen $ R.randomR (0, length xs - 1)
    put q'
    pure $ xs !! a
  PickRandoms n xs -> do
    as <- gets @R.StdGen $ R.randomRs (0, length xs - 1)
    put $ snd $ R.split q
    pure ((xs !!) <$> take n as)
  GetGen -> get @R.StdGen
  SetGen q' -> put q'
