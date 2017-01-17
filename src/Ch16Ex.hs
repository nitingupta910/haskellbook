{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr

data Bool =
  False | True

{- no valid functor for Bool
instance Functor Bool where
  fmap = undefined
-}

data BoolAndSomethingElse a =
  False'a | True' a

instance Functor BoolAndSomethingElse where
  fmap = undefined

newtype Mu f = Inf { outF :: f (Mu f) }

{- no valid functor for Mu since Mu has kind:
     (* -> *) -> *
   while the type for which Functor is to be defined
   must have kind:
     * -> *

instance Functor Mu where
  fmap = undefined
-}

data D = D (Array Word Word) Int Int

{- D has kind *, so no functor can be written for it.
instance Functor D where
  fmap = undefined
-}

data Sum b a
  = First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

data Company a c b
  = DeepBlue a
             c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a
  = L a
      b
      a
  | R b
      a
      b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- 1
data Quant a b
  = Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2
newtype K a b =
  K a

instance Functor (K a) where
  fmap f (K a) = K a

-- 3
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))

-- 4
data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5
data LiftItOut f a  =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut t) = LiftItOut (fmap f t)

-- 6
data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa k k') = DaWrappa (fmap f k) (fmap f k')

-- 7
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething k k') = IgnoringSomething k (fmap f k')

-- 8
data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious k k' k'') = Notorious k k' (fmap f k'')

-- 9
data List a
  = Nil
  | Cons a
         (List a)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a k) = (Cons $ f a) (fmap f k)

-- 10
data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoat (GoatLord a)
             (GoatLord a)
             (GoatLord a)

instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoat k k' k'') = MoreGoat (fmap f k) (fmap f k') (fmap f k'')

-- 11
data TalkToMe a
  = Halt
  | Print String
          a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read f') = Read (f . f')
