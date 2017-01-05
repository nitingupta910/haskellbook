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

type FourAssoc a b c d = (Four a b c d) -> (Four a b c d) -> (Four a b c d) -> Bool

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

type OrAssoc a b = (Or a b) -> (Or a b) -> (Or a b) -> Bool

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Fst a), (1, return $ Snd b)]

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc Int)
  quickCheck (semigroupAssoc :: TwoAssoc Int Int)
  quickCheck (semigroupAssoc :: ThreeAssoc Int Int Int)
  quickCheck (semigroupAssoc :: FourAssoc Int Int Int Int)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc Int Int)
