module Ch15 where

-- CHAPTER 15. MONOID, SEMIGROUP
import           Control.Monad
import           Data.Monoid
import           Test.QuickCheck

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a =>
         Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada          = Nada
  mappend Nada (Only a)      = Only a
  mappend (Only a) Nada      = Only a
  mappend (Only a) (Only a') = Only (mappend a a')

-- Check for monoid laws
monoidAssoc
  :: (Eq m, Monoid m)
  => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity
  :: (Eq m, Monoid m)
  => m -> Bool
monoidLeftIdentity m = (mempty <> m) == m

monoidRightIdentity
  :: (Eq m, Monoid m)
  => m -> Bool
monoidRightIdentity m = (m <> mempty) == m

-- Test above 3 properties/laws on a Monoid
data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

-- Ex: Maybe another Monoid
newtype First' a = First'
  { getFirst' :: Optional a
  } deriving (Eq, Show)

instance (Arbitrary a) =>
         Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nada), (1, return (Only a))]

instance (Arbitrary a) =>
         Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    return (First' a)

instance (Monoid a) => Monoid (First' a) where
  mempty = First' Nada
  mappend (First' a) (First' b) = First' (a <> b)

--firstMappend :: First' a -> First' a -> First' a
--firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
  putStrLn "\nCheck monoid assoc:"
  quickCheck (monoidAssoc :: String -> String -> String -> Bool)
  putStrLn "\nCheck monoid left ident:"
  quickCheck (monoidLeftIdentity :: String -> Bool)
  putStrLn "\nCheck monoid right ident:"
  quickCheck (monoidRightIdentity :: String -> Bool)
  putStrLn "\nCheck monoid assoc for bull"
  quickCheck (monoidAssoc :: BullMappend)
  putStrLn "\nCheck monoid left assoc for bull"
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  putStrLn "\nCheck monoid right assoc for bull"
  quickCheck (monoidRightIdentity :: Bull -> Bool)
  putStrLn "\nCheck assoc for First':"
  quickCheck (monoidAssoc :: FirstMappend)
  putStrLn "\nCheck left ident for First':"
  quickCheck (monoidLeftIdentity :: FstId)
  putStrLn "\nCheck right ident for First':"
  quickCheck (monoidRightIdentity :: FstId)
