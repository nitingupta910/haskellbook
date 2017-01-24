import Data.Monoid
import Control.Applicative
import Data.List (elemIndex)

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]
h z = lookup z [(2, 3), (5, 6), (7, 8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

xs = [1, 2, 3]
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 4 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x'' <*> y'')

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

newtype Constant a b =
  Constant { getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant { getConstant = mempty }
  (<*>) (Constant f) (Constant b) = Constant (f `mappend` b)


-- Data.Monoid also defined a Sum type
newtype Sum' = Sum'
  { getSum' :: Int
  } deriving (Eq, Show)

instance Monoid Sum' where
  mempty = Sum' 0
  mappend (Sum' a) (Sum' b) = Sum' (a + b)

-- 17.5  exs
ex1 = const <$> Just "Hello" <*> pure "World"
ex2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tirerness" <*> pure [1, 2, 3]


-- List Applicative ex
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)


instance Applicative List where
  pure a = Cons a Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) lst = append (fmap f lst) (fs <*> lst)

-- hints for Applicative List
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as
-- end hint
