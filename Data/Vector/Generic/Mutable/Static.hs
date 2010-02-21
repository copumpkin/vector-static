{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
module Data.Vector.Generic.Mutable.Static where

import Control.Monad
import Control.Monad.Primitive

import Data.Vector.Generic.Mutable as GM

import Data.Nat
import Data.Fin

newtype MVec n v s a = MVec { unVec :: v s a }
  deriving (Show, Eq)

overlaps :: MVector v a => MVec n v s a -> MVec n v s a -> Bool
overlaps (MVec x) (MVec y) = GM.overlaps x y

new :: forall m v a n. (PrimMonad m, GM.MVector v a, Nat n) => m (MVec n v (PrimState m) a)
new = liftM MVec (GM.unsafeNew (natToInt (witnessNat :: n)))

newWith :: forall m v a n. (PrimMonad m, GM.MVector v a, Nat n) => a -> m (MVec n v (PrimState m) a)
newWith x = liftM MVec (GM.unsafeNewWith (natToInt (witnessNat :: n)) x)

read :: (PrimMonad m, GM.MVector v a) => MVec n v (PrimState m) a -> Fin n -> m a
read (MVec vs) (Fin i) = GM.unsafeRead vs i

write :: (PrimMonad m, GM.MVector v a) => MVec n v (PrimState m) a -> Fin n -> a -> m ()
write (MVec vs) (Fin i) x = GM.unsafeWrite vs i x

swap :: (PrimMonad m, GM.MVector v a) => MVec n v (PrimState m) a -> Fin n -> Fin n -> m ()
swap (MVec vs) (Fin i) (Fin j) = GM.unsafeSwap vs i j

clear :: (PrimMonad m, MVector v a) => MVec n v (PrimState m) a -> m ()
clear (MVec vs) = GM.clear vs

set :: (PrimMonad m, MVector v a) => MVec n v (PrimState m) a -> a -> m ()
set (MVec vs) x = GM.set vs x

copy :: (PrimMonad m, MVector v a) => MVec n v (PrimState m) a -> MVec n v (PrimState m) a -> m ()
copy (MVec dst) (MVec src) = GM.unsafeCopy dst src

grow :: forall m v a n k. (PrimMonad m, MVector v a, Nat k) => MVec n v (PrimState m) a -> k -> m (MVec (n :+: k) v (PrimState m) a)
grow (MVec vs) k = liftM MVec (GM.unsafeGrow vs (natToInt k))

slice :: (MVector v a, Nat k) => Fin n -> k -> MVec (n :+: k) v s a -> MVec k v s a
slice (Fin i) k (MVec vs) = MVec (GM.unsafeSlice i (natToInt k) vs)

take :: (MVector v a, Nat k) => k -> MVec (n :+: k) v s a -> MVec k v s a
take k (MVec vs) = MVec (GM.take (natToInt k) vs)

drop :: (MVector v a, Nat k) => k -> MVec (n :+: k) v s a -> MVec n v s a
drop k (MVec vs) = MVec (GM.drop (natToInt k) vs)

init :: MVector v a => MVec (S n) v s a -> MVec n v s a
init (MVec vs) = MVec (GM.unsafeInit vs)

tail :: MVector v a => MVec (S n) v s a -> MVec n v s a
tail (MVec vs) = MVec (GM.unsafeTail vs)