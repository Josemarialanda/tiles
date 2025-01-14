module Tiles.Utils.Helpers where

-- Looping

-- | A looping operation, where the predicate returns 'Left' as a seed for the next loop
--   or 'Right' to abort the loop.
--
-- > loop (\x -> if x < 10 then Left $ x * 2 else Right $ show x) 1 == "16"
loop :: (a -> Either a b) -> a -> b
loop act x = case act x of
  Left a  -> loop act a
  Right v -> v

-- | A monadic version of 'loop', where the predicate returns 'Left' as a seed for the next loop
--   or 'Right' to abort the loop.
loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = do
  res <- act x
  case res of
    Left a  -> loopM act a
    Right v -> pure v

fmap2 ::(Functor f1, Functor f2) => (a -> a) -> f1 (f2 a) -> f1 (f2 a)
fmap2 = fmap . fmap

fmap3 :: (Functor f1, Functor f2, Functor f3) => (a -> a) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 a))
fmap3 = fmap . fmap . fmap
