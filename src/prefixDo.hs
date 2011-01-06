#include <h>

untilJust :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
untilJust f [] = return Nothing
untilJust f (x:xs) = do
  y <- f x
  case y of
    Just _ -> return y
    _ -> untilJust f xs

prefixDo :: (Monad m) => ([a] -> m (Maybe b)) -> [a] -> m [b]
prefixDo f [] = return []
prefixDo f xs = do
  let
    xsSplits = reverse . tail $ zip (inits xs) (tails xs)
  Just (y, xsToDo) <- 
    untilJust (\ (a, b) -> liftM (flip (,) b <$>) (f a)) xsSplits
  liftM (y:) $ prefixDo f xsToDo

myF :: String -> IO (Maybe String)
myF s = do
  if length s < 3 
    then
      return $ Just s
    else
      return Nothing

main = do
  r <- prefixDo myF "hello"
  print r
