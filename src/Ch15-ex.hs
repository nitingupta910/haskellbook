import           Data.Semigroup
import           Test.QuickCheck

data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc
  :: (Eq m, Semigroup m)
  => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance (Arbitrary a) =>
         Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance (Semigroup a) =>
         Semigroup (Identity a) where
  (<>) (Identity a) (Identity b) = Identity (a <> b)

type IdentityAssoc a = (Identity a) -> (Identity a) -> (Identity a) -> Bool

instance Semigroup Int where
  (<>) a b = a + b

-- TWO
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Semigroup a, Semigroup b) =>
         Semigroup (Two a b) where
  (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool

-- THREE
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

type ThreeAssoc a b c = (Three a b c) -> (Three a b c) -> (Three a b c) -> Bool

-- FOUR
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (<>) (Four a b c d) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

-- BoolConj
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj True) = BoolConj True
  (<>) _ _                             = BoolConj False

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

instance Arbitrary BoolConj where
  arbitrary = frequency [(1, return (BoolConj True)), (1, return (BoolConj False))]

-- BoolDisj
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False
  (<>) _ _                               = BoolDisj True

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Arbitrary BoolDisj where
  arbitrary = frequency [(1, return (BoolDisj True)), (1, return (BoolDisj False))]

-- Or
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) (Fst a) (Fst a') = Fst a'
  (<>) (Fst a) (Snd a') = Snd a'
  (<>) (Snd a) _        = Snd a

-- OrAssoc
type OrAssoc a b = (Or a b) -> (Or a b) -> (Or a b) -> Bool

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Fst a), (1, return $ Snd b)]

newtype Combine a b = Combine
  { unCombine :: (a -> b)
  }

instance (Semigroup b) =>
         Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine h
    where
      h x = (f x) <> (g x)

newtype Comp a = Comp
  { unComp :: (a -> a)
  }

instance (Semigroup a) =>
         Semigroup (Comp a) where
  (<>) (Comp f) (Comp g) = Comp h
    where
      h x = (f x) <> (g x)

-- Validation
data Validation a b
  = Failure' a
  | Success' b
  deriving (Eq, Show)

-- Why book provides 'Semigroup a' condition here?
instance Semigroup a =>
         Semigroup (Validation a b) where
  (<>) (Failure' a) (Failure' b) = Failure' a
  (<>) (Failure' a) _            = Failure' a
  (<>) (Success' a) _            = Success' a

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Failure' a), (1, return $ Success' b)]

type ValidationAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool

-- AccumulateRight
newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b =>
         Semigroup (AccumulateRight a b) where
  (<>) (AccumulateRight (Success' a)) (AccumulateRight (Success' b)) = AccumulateRight (Success' (a <> b))
  (<>) (AccumulateRight (Success' a)) (AccumulateRight (Failure' b)) = AccumulateRight (Success' a)
  (<>) (AccumulateRight (Failure' a)) (AccumulateRight (Success' b)) = AccumulateRight (Success' b)
  (<>) (AccumulateRight (Failure' a)) (AccumulateRight (Failure' b)) = AccumulateRight (Failure' a)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (AccumulateRight a b) where
  arbitrary = do
    v <- arbitrary
    return $ AccumulateRight v

type AccumulateRightAssoc a b = AccumulateRight a b -> AccumulateRight a b -> AccumulateRight a b -> Bool

-- AccumulateBoth
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
         Semigroup (AccumulateBoth a b) where
  (<>) (AccumulateBoth (Success' a)) (AccumulateBoth (Success' b)) = AccumulateBoth (Success' $ a <> b)
  (<>) (AccumulateBoth (Success' a)) (AccumulateBoth (Failure' b)) = AccumulateBoth (Success' $ a)
  (<>) (AccumulateBoth (Failure' a)) (AccumulateBoth (Success' b)) = AccumulateBoth (Success' $ b)
  (<>) (AccumulateBoth (Failure' a)) (AccumulateBoth (Failure' b)) = AccumulateBoth (Failure' $ a <> b)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    v <- arbitrary
    return $ AccumulateBoth v

type AccumulateBothAssoc a b = AccumulateBoth a b -> AccumulateBoth a b -> AccumulateBoth a b -> Bool

-- MONOID EXERCISES
monoidLeftIdentity
  :: (Eq m, Semigroup m, Monoid m)
  => m -> Bool
monoidLeftIdentity m = (mempty <> m) == m

monoidRightIdentity
  :: (Eq m, Semigroup m, Monoid m)
  => m -> Bool
monoidRightIdentity m = (m <> mempty) == m

-- Trivial
instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

-- Identity
instance (Semigroup a, Monoid a) =>
         Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

-- BoolConj
instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

-- BoolDisj
instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

-- Combine
instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine (\_ -> mempty)
  mappend = (<>)

-- Comp
instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)

-- Mem
newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }

instance (Semigroup a, Monoid a) => Semigroup (Mem s a) where
  (<>) (Mem f) (Mem g) = Mem h where
      h = \x -> ((fst $ f x) <> (fst $ g x), snd $ g $ snd $ f x)

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  putStrLn "Monoid checks:\n"
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc Int)
  quickCheck (semigroupAssoc :: TwoAssoc Int Int)
  quickCheck (semigroupAssoc :: ThreeAssoc Int Int Int)
  quickCheck (semigroupAssoc :: FourAssoc Int Int Int Int)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc Int Int)
  quickCheck (semigroupAssoc :: ValidationAssoc Int Int)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc Int Int)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc Int Int)
  putStrLn "\n\nMonad checks:\n"
  putStrLn "\nTrivial:"
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  putStrLn "\nIdentity:"
  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  putStrLn "\nBoolConj:"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  putStrLn "\nBoolDisj:"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  putStrLn "\nMem:"
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
