{-# LANGUAGE RankNTypes, TypeOperators, TypeFamilies, ScopedTypeVariables, FlexibleContexts, UndecidableInstances, FlexibleInstances #-}
module Data.Nat where

import Unsafe.Coerce

-- Adapted from Ryan Ingram's http://www.mail-archive.com/haskell-cafe@haskell.org/msg60806.html

newtype I x   = I { unI :: x }
newtype K x y = K { unK :: x }

-- Same representation, can be coerced at no cost. Probably buys us nothing though, as we must add to maintain invariants.
newtype Z   = Z Int
newtype S n = S Int

z :: Z
z = Z 0

s :: Nat n => n -> S n
s n = unsafeCoerce (unsafeCoerce n + (1 :: Int))

class Nat n where
  natToInt :: n -> Int
  caseNat  :: forall r. n -> (n ~ Z => r) -> (forall p. (n ~ S p, Nat p) => p -> r) -> r

instance Nat Z where
  natToInt _ = 0
  caseNat  _ z _ = z

instance Nat n => Nat (S n) where
  natToInt = unsafeCoerce
  caseNat n _ f = f (unsafeCoerce (unsafeCoerce n - (1 :: Int)))

induction :: forall p n. Nat n => n -> p Z -> (forall x. Nat x => p x -> p (S x)) -> p n
induction n z s = caseNat n isZ isS where
  isZ :: n ~ Z => p n
  isZ = z
  isS :: forall x. (n ~ S x, Nat x) => x -> p n
  isS x = s (induction x z s)

-- Expensive!!
witnessNat :: forall n. Nat n => n
witnessNat = theWitness where
  theWitness = unI $ induction (undefined `asTypeOf` theWitness) (I z) (I . s . unI)

type family (:+:) a b :: *
type instance (:+:) Z n = n
type instance (:+:) (S m) n = S (m :+: n)

type family (:*:) a b :: *
type instance (:*:) Z n = Z
type instance (:*:) (S m) n = n :+: (m :*: n)

addNat :: (Nat a, Nat b) => a -> b -> a :+: b
addNat a b = unsafeCoerce (natToInt a + natToInt b)

mulNat :: (Nat a, Nat b) => a -> b -> a :*: b
mulNat a b = unsafeCoerce (natToInt a * natToInt b)