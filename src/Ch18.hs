import Control.Monad (join)

-- bind in terms of fmap and join
bind
  :: Monad m
  => (a -> m b) -> m a -> m b
bind f ma = join $ fmap f ma

-- do syntax make things a bit cleaner and easier to read
twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y halo thar: " ++ name ++ " who is: " ++ age ++ " years old")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >> getLine >>=
  \name ->
     putStrLn "age pls:" >> getLine >>=
     \age ->
        putStrLn ("y halo thar: " ++ name ++ " who is: " ++ age ++ " years old")

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else [x * x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []
