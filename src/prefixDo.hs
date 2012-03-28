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

fakeUtf8 = map (chr . fromIntegral) . BS.unpack . DTE.encodeUtf8 . DT.pack

myF :: String -> IO (Maybe BS.ByteString)
myF [] = return Nothing
myF s = do
  ls <- run ("z", ["-e", fakeUtf8 s]) :: IO BS.ByteString
  if BS.null ls
    then
      if length s <= 1
        then return $ Just $ BSC.pack s
        else return Nothing
    else do
      --BS.putStr ls
      return $ Just ls

f s = mapM_ BS.putStr . nub =<< prefixDo myF s

--getPinyin = BSC.takeWhile (/= ']') . BS.tail . BSC.dropWhile (/= '[')
getPinyin = id

main = do
  wds <- map (filter (not . isAscii)) . words <$> readFile "songlyr.txt"
  r <- map getPinyin . concat <$> mapM (prefixDo myF) wds
  mapM_ BS.putStrLn r
  {-
  r <- nub . concat <$> mapM (prefixDo myF) wds
  mapM_ BS.putStrLn r
  -}

{-
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
-}
