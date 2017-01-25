import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- pair
data Pair a =
  Pair a
       a
  deriving (Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

-- two
data Two a b =
  Two a
      b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a =>
         Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two x f) (Two x' a') = Two (x <> x') (f a')

-- three
data Three a b c =
  Three a
        b
        c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) =>
         Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a b f) (Three a' b' x) = Three (a <> a') (b <> b') (f x)

-- three'
data Three' a b =
  Three' a
         b
         b

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) =>
         Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a f f') (Three' a' x x') = Three' (a <> a') (f x) (f' x')

-- four
data Four a b c d =
  Four a
       b
       c
       d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) =>
         Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four a b c f) (Four a' b' c' x) =
    Four (a <> a') (b <> b') (c <> c') (f x)

-- four'
data Four' a b =
  Four' a
        a
        a
        b

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a =>
         Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' a1 a2 a3 f) (Four' a1' a2' a3' x) =
    Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f x)

-- combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos fa fb fc = liftA3 (,,) fa fb fc
