{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

------------------------------------------------
-- The Abstract monad
------------------------------------------------

data Abstract a = Top
                | Bottom
                | Abstract a
                deriving Show

instance Functor Abstract where
  fmap _ Top          = Top
  fmap _ Bottom       = Bottom
  fmap f (Abstract x) = Abstract (f x)

instance Applicative Abstract where
  Top          <*> _ = Top
  Bottom       <*> _ = Bottom
  (Abstract f) <*> x = fmap f x

  pure = Abstract

instance Monad Abstract where
  Top          >>= f = Top
  Bottom       >>= f = Bottom
  (Abstract x) >>= f = f x

  return = Abstract

------------------------------------------------
-- The LispVal type
------------------------------------------------

data LispVal m = Nil
               | Integer Integer
               | Boolean Bool
               | Atom String
               | Cons (m (LispVal m)) (m (LispVal m))

deriving instance Show (m (LispVal m)) => Show (LispVal m)

instance Num (LispVal m) where
  (Integer x) + (Integer y) = Integer (x + y)
  _           + _           = undefined

  (Integer x) * (Integer y) = Integer (x + y)
  _           * _           = undefined

  abs (Integer x) = Integer (abs x)
  abs _           = undefined

  signum (Integer x) = Integer (signum x)
  signum _           = undefined

  fromInteger x = Integer x

  negate (Integer x) = Integer (negate x)
  negate _           = undefined

------------------------------------------------
-- Misc. function definitions
------------------------------------------------

cons :: Monad m => LispVal m -> LispVal m -> LispVal m
cons x y = Cons (return x) (return y)

car :: LispVal m -> m (LispVal m)
car (Cons x _) = x
car _          = undefined

cdr :: LispVal m -> m (LispVal m)
cdr (Cons _ x) = x
cdr _          = undefined
