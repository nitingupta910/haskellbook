module Ch16 where

import Test.QuickCheck
import Test.QuickCheck.Function

replaceWithP :: a -> Char
replaceWithP = const 'p'

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

-- lmls ~ List (Maybe (List String))
ha = Just ["Ha", "Ha"]
lmls = [ha, Nothing, Just []]

f1 :: [Maybe [[Char]]] -> [Char]
f1 = fmap replaceWithP

f2 :: [Maybe [[Char]]] -> [Maybe Char]
f2 = (fmap . fmap) replaceWithP

f3 :: [Maybe [[Char]]] -> [Maybe [Char]]
f3 = (fmap . fmap . fmap) replaceWithP

f4 :: [Maybe [[Char]]] -> [Maybe [[Char]]]
f4 = (fmap . fmap . fmap . fmap) replaceWithP

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- Prelude> :t fmap replaceWithP
-- fmap replaceWithP :: Functor f => f a -> f Char

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- we can assert more specific type for liftedReplace
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

-- Prelude> :t (fmap . fmap) replaceWithP
-- (fmap . fmap) replaceWithP
--   :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- Making it more specific
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted
-- f  ~ []
-- f1 ~ Maybe

-- Prelude> :t (fmap . fmap . fmap) replaceWithP
-- (fmap . fmap . fmap) replaceWithP
--   :: (Functor f2, Functor f1, Functor f) =>
--     f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- More specific or "concrete"
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted
-- f  ~ []
-- f1 ~ Maybe
-- f2 ~ []

-- exs: heavy lifting
a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = (*2) . (\x -> x - 2)

d = ((return '1' ++) . show) . (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read (fmap ("123" ++) (fmap show ioi))
    in fmap (*3) changed
-- end exs: heavy lifting

-- Functor laws
-- fmap id      = id
-- fmap (p . q) = (fmap p) . (fmap q)

-- Turn these laws into QuickCheck properties

functorIdentity
  :: (Functor f, Eq (f a))
  => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose
  :: (Eq (f c), Functor f)
  => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == fmap (g . f) x

functorCompose'
  :: (Eq (f c), Functor f)
  => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

-- 16.10 exs: Instances of Func

-- identity
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance (Arbitrary a) =>
         Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

type IdentityFC = (Identity Int) -> IntToInt -> IntToInt -> Bool

-- pair
data Pair a = Pair a a deriving (Eq, Show)

instance (Arbitrary a) =>
         Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

type PairFC = (Pair Int) -> IntToInt -> IntToInt -> Bool

-- two
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

type TwoFC = (Two Int Int) -> IntToInt -> IntToInt -> Bool


-- three
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

type ThreeFC = Three Int Int Int -> IntToInt -> IntToInt -> Bool

-- three'
data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

type Three'FC = Three' Int Int -> IntToInt -> IntToInt -> Bool

-- four
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

type FourFC = (Four Int Int Int Int) -> IntToInt -> IntToInt -> Bool

-- four'
data Four' a b = Four' a a a b

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

type Four'FC = Four' Int Int -> IntToInt -> IntToInt -> Bool

-- trivial
data Trivial = Trivial
-- can't implement functor for this type since its kind is *
-- while functors can be implemented only for types with kind
-- * -> *

-- end exs: Instances of Func

main :: IO ()
main = do
  putStr "replaceWithP' lms:   "
  print (replaceWithP' lms)

  putStr "liftedReplace lms:   "
  print (liftedReplace lms)

  putStr "liftedReplace' lms:  "
  print (liftedReplace' lms)

  putStr "twiceLifted lms:     "
  print (twiceLifted lms)

  putStr "twiceLifted' lms:    "
  print (twiceLifted' lms)

  putStr "thriceLifted lms:    "
  print (thriceLifted lms)

  putStr "thriceLifted' lms:   "
  print (thriceLifted' lms)

  quickCheck $ \x -> functorIdentity (x :: [Int])

  let li x = functorCompose (+1) (*2) (x :: [Int])
  quickCheck li

  quickCheck (functorCompose' :: IntFC)

  putStrLn "\n16.10 Exercises: Instances of Func"
  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck (functorCompose' :: IdentityFC)

  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck (functorCompose' :: PairFC)

  quickCheck $ \x -> functorIdentity (x :: Two Int Int)
  quickCheck (functorCompose' :: TwoFC)

  quickCheck $ \x -> functorIdentity (x :: Three Int Int Int)
  quickCheck (functorCompose' :: ThreeFC)

  quickCheck $ \x -> functorIdentity (x :: Three' Int Int)
  quickCheck (functorCompose' :: Three'FC)

  quickCheck $ \x -> functorIdentity (x :: Four Int Int Int Int)
  quickCheck (functorCompose' :: FourFC)

