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

data Quant a b
  = Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K' a b =
  K' a

instance Functor (K' a) where
  fmap f (K' a) = K' a

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b =
  K a  

instance Functor (Flip K a) where
  fmap f (Flip f' b a) = Flip 
