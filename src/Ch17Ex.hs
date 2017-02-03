import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- pair
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance (Arbitrary a) =>
         Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    elements [Pair a a']

instance Eq a =>
         EqProp (Pair a) where
  (=-=) = eq

type S = String

-- two
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a =>
         Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two x f) (Two x' a') = Two (x <> x') (f a')

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Two a b]

instance (Eq a, Eq b) =>
         EqProp (Two a b) where
  (=-=) = eq

-- three
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) =>
         Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a b f) (Three a' b' x) = Three (a <> a') (b <> b') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    elements [Three a b c]

instance (Eq a, Eq b, Eq c) =>
         EqProp (Three a b c) where
  (=-=) = eq

-- three'
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) =>
         Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a f f') (Three' a' x x') = Three' (a <> a') (f x) (f' x')

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    elements [Three' a b b']

instance (Eq a, Eq b) =>
         EqProp (Three' a b) where
  (=-=) = eq

-- four
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) =>
         Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four a b c f) (Four a' b' c' x) =
    Four (a <> a') (b <> b') (c <> c') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    elements [Four a b c d]

instance (Eq a, Eq b, Eq c, Eq d) =>
         EqProp (Four a b c d) where
  (=-=) = eq

-- four'
data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    elements [Four' a a' a'' b]

instance (Eq a, Eq b) =>
         EqProp (Four' a b) where
  (=-=) = eq

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

main :: IO ()
main = do
  putStr "\nPair"
  quickBatch $ applicative (undefined :: Pair (S, S, S))
  putStr "\nTwo"
  quickBatch $ applicative (undefined :: Two S (S, S, S))
  putStr "\nThree"
  quickBatch $ applicative (undefined :: Three S S (S, S, S))
  putStr "\nThree'"
  quickBatch $ applicative (undefined :: Three' S (S, S, S))
  putStr "\nFour"
  quickBatch $ applicative (undefined :: Four S S S (S, S, S))
  putStr "\nFour'"
  quickBatch $ applicative (undefined :: Four' S (S, S, S))
