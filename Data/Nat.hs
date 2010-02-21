{-# LANGUAGE RankNTypes, TypeOperators, TypeFamilies, ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}
module Data.Nat where

-- Borrowed from Ryan Ingram's http://www.mail-archive.com/haskell-cafe@haskell.org/msg60806.html

newtype I x   = I { unI :: x }
newtype K x y = K { unK :: x }

data Z = Z
newtype S n = S n

class Nat n where
  caseNat :: forall r. n -> (n ~ Z => r) -> (forall p. (n ~ S p, Nat p) => p -> r) -> r

instance Nat Z where
  caseNat _ z _ = z
  
instance Nat n => Nat (S n) where
  caseNat (S n) _ s = s n

induction :: forall p n. Nat n => n -> p Z -> (forall x. Nat x => p x -> p (S x)) -> p n
induction n z s = caseNat n isZ isS where
  isZ :: n ~ Z => p n
  isZ = z
  isS :: forall x. (n ~ S x, Nat x) => x -> p n
  isS x = s (induction x z s)

natToInt :: Nat n => n -> Int
natToInt n = unK $ induction n (K 0) (K . (+1) . unK)

witnessNat :: forall n. Nat n => n
witnessNat = theWitness where
  theWitness = unI $ induction (undefined `asTypeOf` theWitness) (I Z) (I . S . unI)

type family (:+:) a b :: *
type instance (:+:) Z n = n
type instance (:+:) (S m) n = S (m :+: n)

type family (:*:) a b :: *
type instance (:*:) Z n = Z
type instance (:*:) (S m) n = (m :*: n) :*: n
