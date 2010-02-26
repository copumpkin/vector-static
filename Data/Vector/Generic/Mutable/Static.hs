{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
module Data.Vector.Generic.Mutable.Static where

import Control.Monad
import Control.Monad.Primitive

import Data.Vector.Generic.Mutable as GM

import Data.Nat
import Data.Fin

newtype MVec n v s a = MVec { unVec :: v s a }
  deriving (Show, Eq)

-- length?

overlaps :: MVector v a => MVec n v s a -> MVec n v s a -> Bool
overlaps (MVec x) (MVec y) = GM.overlaps x y

new :: forall m v a n. (PrimMonad m, GM.MVector v a, Nat n) => n -> m (MVec n v (PrimState m) a)
new n = liftM MVec (GM.unsafeNew (natToInt n))

newWith :: forall m v a n. (PrimMonad m, GM.MVector v a, Nat n) => n -> a -> m (MVec n v (PrimState m) a)
newWith n x = liftM MVec (GM.unsafeNewWith (natToInt n) x)

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

{-
unstream :: (PrimMonad m, MVector v a) => Stream a -> m (MVec n v (PrimState m) a)
unstream = undefined

transform :: (PrimMonad m, MVector v a) => (MStream m a -> MStream m a) -> v (PrimState m) a -> m (v (PrimState m) a)
transform = undefined

unstreamR :: (PrimMonad m, MVector v a) => Stream a -> m (v (PrimState m) a)
unstreamR = undefined

transformR :: (PrimMonad m, MVector v a) => (MStream m a -> MStream m a) -> v (PrimState m) a -> m (v (PrimState m) a)
transformR = undefined

accum :: (PrimMonad m, MVector v a) => (a -> b -> a) -> v (PrimState m) a -> Stream (Int, b) -> m ()
accum = undefined

update :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Stream (Int, a) -> m ()
update = undefined

reverse :: (PrimMonad m, MVector v a) => v (PrimState m) a -> m ()
reverse = undefined

unstablePartition :: forall m v a. (PrimMonad m, MVector v a) => (a -> Bool) -> v (PrimState m) a -> m Int
unstablePartition = undefined

unstablePartitionStream :: (PrimMonad m, MVector v a) => (a -> Bool) -> Stream a -> m (v (PrimState m) a, v (PrimState m) a)
unstablePartitionStream = undefined

partitionStream :: (PrimMonad m, MVector v a) => (a -> Bool) -> Stream a -> m (v (PrimState m) a, v (PrimState m) a)
partitionStream = undefined
-}