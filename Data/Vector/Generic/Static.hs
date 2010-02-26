{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, TypeFamilies, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances #-}
module Data.Vector.Generic.Static where

import Control.Applicative

import Prelude hiding (map, take, drop, concatMap)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import Unsafe.Coerce

import Data.Nat
import Data.Fin

import Data.Vector.Fusion.Stream (Stream)
import Data.Vector.Generic.New (New)

newtype Vec n v a = Vec { unVec :: v a }
  deriving (Show, Eq)


length :: (G.Vector v a, Nat n) => Vec n v a -> n
{-# INLINE length #-}
length = unsafeCoerce . G.length . unVec

-- null

empty :: G.Vector v a => Vec Z v a
{-# INLINE empty #-}
empty = Vec G.empty

singleton :: G.Vector v a => a -> Vec (S Z) v a
{-# INLINE singleton #-}
singleton = Vec . G.singleton

cons :: G.Vector v a => a -> Vec n v a -> Vec (S n) v a
{-# INLINE cons #-}
cons x (Vec xs) = Vec (G.cons x xs)

snoc :: G.Vector v a => Vec n v a -> a -> Vec (S n) v a
{-# INLINE snoc #-}
snoc (Vec xs) x = Vec (G.snoc xs x)

replicate :: forall a n v. (Nat n, G.Vector v a) => n -> a -> Vec n v a
{-# INLINE replicate #-}
replicate n = Vec . G.replicate (natToInt n)

generate :: forall n v a. (Nat n, G.Vector v a) => n -> (Fin n -> a) -> Vec n v a
{-# INLINE generate #-}
generate n f = Vec (G.generate (natToInt n) (f . Fin))

(++) :: G.Vector v a => Vec m v a -> Vec n v a -> Vec (m :+: n) v a
{-# INLINE (++) #-}
Vec ms ++ Vec ns = Vec (ms G.++ ns)

copy :: G.Vector v a => Vec n v a -> Vec n v a
{-# INLINE copy #-}
copy (Vec vs) = Vec (G.copy vs)

(!) :: G.Vector v a => Vec n v a -> Fin n -> a
{-# INLINE (!) #-}
Vec vs ! Fin i = G.unsafeIndex vs i

head :: G.Vector v a => Vec (S n) v a -> a
{-# INLINE head #-}
head (Vec vs) = G.unsafeHead vs

last :: G.Vector v a => Vec (S n) v a -> a
{-# INLINE last #-}
last (Vec vs) = G.unsafeLast vs

-- indexM
-- headM
-- lastM

slice :: (G.Vector v a, Nat k) => Fin n -> k -> Vec (n :+: k) v a -> Vec k v a
{-# INLINE slice #-}
slice (Fin i) k (Vec vs) = Vec (G.unsafeSlice i (natToInt k) vs)

init :: G.Vector v a => Vec (S n) v a -> Vec n v a
{-# INLINE init #-}
init (Vec vs) = Vec (G.unsafeInit vs)

tail :: G.Vector v a => Vec (S n) v a -> Vec n v a
{-# INLINE tail #-}
tail (Vec vs) = Vec (G.unsafeTail vs)

take :: (G.Vector v a, Nat k) => k -> Vec (n :+: k) v a -> Vec k v a
{-# INLINE take #-}
take k (Vec vs) = Vec (G.take (natToInt k) vs)

drop :: (G.Vector v a, Nat k) => k -> Vec (n :+: k) v a -> Vec n v a
{-# INLINE drop #-}
drop k (Vec vs) = Vec (G.drop (natToInt k) vs)

-- splitAt?

-- accum
-- accumulate
-- accumulate_
-- (//)
-- update
-- update_

backpermute :: (G.Vector v a, G.Vector v Int) => Vec m v a -> Vec n v (Fin m) -> Vec n v a
{-# INLINE backpermute #-}
backpermute (Vec vs) (Vec is) = Vec (G.unsafeBackpermute vs (unsafeCoerce is))

reverse :: G.Vector v a => Vec n v a -> Vec n v a
{-# INLINE reverse #-}
reverse (Vec vs) = Vec (G.reverse vs)

map :: (G.Vector v a, G.Vector v b) => (a -> b) -> Vec n v a -> Vec n v b
{-# INLINE map #-}
map f (Vec vs) = Vec (G.map f vs)

imap :: (G.Vector v a, G.Vector v b) => (Fin n -> a -> b) -> Vec n v a -> Vec n v b
{-# INLINE imap #-}
imap f (Vec vs) = Vec (G.imap (f . Fin) vs)

concatMap :: (G.Vector v a, G.Vector v b) => (a -> Vec n v b) -> Vec m v a -> Vec (m :*: n) v b
{-# INLINE concatMap #-}
concatMap f (Vec as) = Vec (G.concatMap (unVec . f) as)

zipWith :: (G.Vector v a, G.Vector v b, G.Vector v c) => (a -> b -> c) -> Vec n v a -> Vec n v b -> Vec n v c
{-# INLINE zipWith #-}
zipWith f (Vec as) (Vec bs) = Vec (G.zipWith f as bs)

zipWith3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d) => (a -> b -> c -> d) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d
{-# INLINE zipWith3 #-}
zipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.zipWith3 f as bs cs)

zipWith4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e) => (a -> b -> c -> d -> e) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e
{-# INLINE zipWith4 #-}
zipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.zipWith4 f as bs cs ds)

zipWith5 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v f) => (a -> b -> c -> d -> e -> f) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v f
{-# INLINE zipWith5 #-}
zipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.zipWith5 f as bs cs ds es)

zipWith6 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v f, G.Vector v g) => (a -> b -> c -> d -> e -> f -> g) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v f -> Vec n v g
{-# INLINE zipWith6 #-}
zipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.zipWith6 f as bs cs ds es fs)


izipWith :: (G.Vector v a, G.Vector v b, G.Vector v c) => (Fin n -> a -> b -> c) -> Vec n v a -> Vec n v b -> Vec n v c
{-# INLINE izipWith #-}
izipWith f (Vec as) (Vec bs) = Vec (G.izipWith (f . Fin) as bs)

izipWith3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d) => (Fin n -> a -> b -> c -> d) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d
{-# INLINE izipWith3 #-}
izipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.izipWith3 (f . Fin) as bs cs)

izipWith4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e) => (Fin n -> a -> b -> c -> d -> e) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e
{-# INLINE izipWith4 #-}
izipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.izipWith4 (f . Fin) as bs cs ds)

izipWith5 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v f) => (Fin n -> a -> b -> c -> d -> e -> f) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v f
{-# INLINE izipWith5 #-}
izipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.izipWith5 (f . Fin) as bs cs ds es)

izipWith6 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v f, G.Vector v g) => (Fin n -> a -> b -> c -> d -> e -> f -> g) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v f -> Vec n v g
{-# INLINE izipWith6 #-}
izipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.izipWith6 (f . Fin) as bs cs ds es fs)


zip :: (G.Vector v a, G.Vector v b, G.Vector v (a, b)) => Vec n v a -> Vec n v b -> Vec n v (a, b)
{-# INLINE zip #-}
zip (Vec as) (Vec bs) = Vec (G.zip as bs)
   
zip3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v (a, b, c)) => Vec n v a -> Vec n v b -> Vec n v c -> Vec n v (a, b, c)
{-# INLINE zip3 #-}
zip3 (Vec as) (Vec bs) (Vec cs) = Vec (G.zip3 as bs cs)
   
zip4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v (a, b, c, d)) => Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v (a, b, c, d)
{-# INLINE zip4 #-}
zip4 (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.zip4 as bs cs ds)
   
zip5 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v (a, b, c, d, e)) => Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v (a, b, c, d, e)
{-# INLINE zip5 #-}
zip5 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.zip5 as bs cs ds es)
   
zip6 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v f, G.Vector v (a, b, c, d, e, f)) => Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v f -> Vec n v (a, b, c, d, e, f)
{-# INLINE zip6 #-}
zip6 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.zip6 as bs cs ds es fs)


unzip :: (G.Vector v a, G.Vector v b, G.Vector v (a, b)) => Vec n v (a, b) -> (Vec n v a, Vec n v b)
{-# INLINE unzip #-}
unzip (Vec vs) = (Vec as, Vec bs)
  where (as, bs) = G.unzip vs

unzip3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v (a, b, c)) => Vec n v (a, b, c) -> (Vec n v a, Vec n v b, Vec n v c)
{-# INLINE unzip3 #-}
unzip3 (Vec vs) = (Vec as, Vec bs, Vec cs)
  where (as, bs, cs) = G.unzip3 vs

unzip4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v (a, b, c, d)) => Vec n v (a, b, c, d) -> (Vec n v a, Vec n v b, Vec n v c, Vec n v d)
{-# INLINE unzip4 #-}
unzip4 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds)
  where (as, bs, cs, ds) = G.unzip4 vs

unzip5 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v (a, b, c, d, e)) => Vec n v (a, b, c, d, e) -> (Vec n v a, Vec n v b, Vec n v c, Vec n v d, Vec n v e)
{-# INLINE unzip5 #-}
unzip5 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds, Vec es)
  where (as, bs, cs, ds, es) = G.unzip5 vs

unzip6 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v f, G.Vector v (a, b, c, d, e, f)) => Vec n v (a, b, c, d, e, f) -> (Vec n v a, Vec n v b, Vec n v c, Vec n v d, Vec n v e, Vec n v f)
{-# INLINE unzip6 #-}
unzip6 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds, Vec es, Vec fs)
  where (as, bs, cs, ds, es, fs) = G.unzip6 vs

-- filter
-- ifilter
-- takeWhile
-- dropWhile
-- partition
-- unstablePartition
-- span
-- break

elem :: (G.Vector v a, Eq a) => a -> Vec n v a -> Bool
{-# INLINE elem #-}
elem x (Vec vs) = G.elem x vs

notElem :: (G.Vector v a, Eq a) => a -> Vec n v a -> Bool
{-# INLINE notElem #-}
notElem x (Vec vs) = G.notElem x vs

find :: (G.Vector v a, Eq a) => (a -> Bool) -> Vec n v a -> Maybe a
{-# INLINE find #-}
find p (Vec vs) = G.find p vs

findIndex :: G.Vector v a => (a -> Bool) -> Vec n v a -> Maybe (Fin n)
{-# INLINE findIndex #-}
findIndex p (Vec vs) = fmap Fin $ G.findIndex p vs

findIndices :: (G.Vector v a, G.Vector v Int, G.Vector v (Fin n)) => (a -> Bool) -> Vec n v a -> Vec m v (Fin n) -- should we return proof that m <= n? Maybe just return k and m such that m + k = n.
{-# INLINE findIndices #-}
findIndices p (Vec vs) = Vec (G.map Fin $ G.findIndices p vs)

elemIndex :: G.Vector v a => Eq a => a -> Vec n v a -> Maybe (Fin n)
{-# INLINE elemIndex #-}
elemIndex x (Vec vs) = fmap Fin $ G.elemIndex x vs

elemIndices :: (G.Vector v a, G.Vector v Int, G.Vector v (Fin n)) => Eq a => a -> Vec n v a -> Vec m v (Fin n)
{-# INLINE elemIndices #-}
elemIndices x (Vec vs) = Vec (G.map Fin $ G.elemIndices x vs)

foldl :: G.Vector v b => (a -> b -> a) -> a -> Vec n v b -> a
{-# INLINE foldl #-}
foldl f z (Vec vs) = G.foldl f z vs

foldl1 :: G.Vector v a => (a -> a -> a) -> Vec (S n) v a -> a
{-# INLINE foldl1 #-}
foldl1 f (Vec vs) = G.foldl1 f vs

foldl' :: G.Vector v b => (a -> b -> a) -> a -> Vec n v b -> a
foldl' f z (Vec vs) = G.foldl' f z vs

foldl1' :: G.Vector v a => (a -> a -> a) -> Vec (S n) v a -> a
foldl1' f (Vec vs) = G.foldl1' f vs

foldr :: G.Vector v a => (a -> b -> b) -> b -> Vec n v a -> b
{-# INLINE foldr #-}
foldr f z (Vec vs) = G.foldr f z vs

foldr1 :: G.Vector v a => (a -> a -> a) -> Vec (S n) v a -> a
{-# INLINE foldr1 #-}
foldr1 f (Vec vs) = G.foldr1 f vs

foldr' :: G.Vector v a => (a -> b -> b) -> b -> Vec n v a -> b
foldr' f z (Vec vs) = G.foldr' f z vs

foldr1' :: G.Vector v a => (a -> a -> a) -> Vec (S n) v a -> a
foldr1' f (Vec vs) = G.foldr1' f vs

ifoldl :: G.Vector v b => (a -> Fin n -> b -> a) -> a -> Vec n v b -> a
{-# INLINE ifoldl #-}
ifoldl f z (Vec vs) = G.ifoldl (\a b -> f a (Fin b)) z vs

ifoldl' :: G.Vector v b => (a -> Fin n -> b -> a) -> a -> Vec n v b -> a
ifoldl' f z (Vec vs) = G.ifoldl' (\a b -> f a (Fin b)) z vs

ifoldr :: G.Vector v a => (Fin n -> a -> b -> b) -> b -> Vec n v a -> b
{-# INLINE ifoldr #-}
ifoldr f z (Vec vs) = G.ifoldr (f . Fin) z vs

ifoldr' :: G.Vector v a => (Fin n -> a -> b -> b) -> b -> Vec n v a -> b
ifoldr' f z (Vec vs) = G.ifoldr' (f . Fin) z vs

all :: G.Vector v a => (a -> Bool) -> Vec n v a -> Bool
{-# INLINE all #-}
all p (Vec vs) = G.all p vs

any :: G.Vector v a => (a -> Bool) -> Vec n v a -> Bool
{-# INLINE any #-}
any p (Vec vs) = G.any p vs

and :: G.Vector v Bool => Vec n v Bool -> Bool
{-# INLINE and #-}
and (Vec vs) = G.and vs

or :: G.Vector v Bool => Vec n v Bool -> Bool
{-# INLINE or #-}
or (Vec vs) = G.or vs

sum :: (G.Vector v a, Num a) => Vec n v a -> a
{-# INLINE sum #-}
sum (Vec vs) = G.sum vs

product :: (G.Vector v a, Num a) => Vec n v a -> a
{-# INLINE product #-}
product (Vec vs) = G.product vs

minimum :: (Ord a, G.Vector v a) => Vec (S n) v a -> a
{-# INLINE minimum #-}
minimum (Vec vs) = G.minimum vs

minimumBy :: G.Vector v a => (a -> a -> Ordering) -> Vec (S n) v a -> a
{-# INLINE minimumBy #-}
minimumBy c (Vec vs) = G.minimumBy c vs

minIndex :: (Ord a, G.Vector v a) => Vec (S n) v a -> Fin (S n)
{-# INLINE minIndex #-}
minIndex (Vec vs) = Fin (G.minIndex vs)

minIndexBy :: G.Vector v a => (a -> a -> Ordering) -> Vec (S n) v a -> Fin (S n)
{-# INLINE minIndexBy #-}
minIndexBy c (Vec vs) = Fin (G.minIndexBy c vs)

maximum :: (Ord a, G.Vector v a) => Vec (S n) v a -> a
{-# INLINE maximum #-}
maximum (Vec vs) = G.maximum vs

maximumBy :: G.Vector v a => (a -> a -> Ordering) -> Vec (S n) v a -> a
{-# INLINE maximumBy #-}
maximumBy c (Vec vs) = G.maximumBy c vs

maxIndex :: (Ord a, G.Vector v a) => Vec (S n) v a -> Fin (S n)
{-# INLINE maxIndex #-}
maxIndex (Vec vs) = Fin (G.maxIndex vs)

maxIndexBy :: G.Vector v a => (a -> a -> Ordering) -> Vec (S n) v a -> Fin (S n)
{-# INLINE maxIndexBy #-}
maxIndexBy c (Vec vs) = Fin (G.maxIndexBy c vs)

unfoldr :: G.Vector v a => (b -> Maybe (a, b)) -> b -> (forall n. Vec n v a -> r) -> r
{-# INLINE unfoldr #-}
unfoldr f x c = c (Vec (G.unfoldr f x))

-- prescanl
-- prescanl'
-- postscanl
-- postscanl'

-- scanl
-- scanl'
-- scanl1
-- scanl1'

-- prescanr
-- prescanr'
-- postscanr
-- postscanr'

-- scanr
-- scanr'
-- scanr1
-- scanr1'

enumFromN :: forall v a n. (G.Vector v a, Num a, Nat n) => a -> n -> Vec n v a
{-# INLINE enumFromN #-}
enumFromN x n = Vec (G.enumFromN x (natToInt n))

enumFromStepN :: forall v a n. (G.Vector v a, Num a, Nat n) => a -> a -> n -> Vec n v a
{-# INLINE enumFromStepN #-}
enumFromStepN x x1 n = Vec (G.enumFromStepN x x1 (natToInt n))

-- enumFromTo
-- enumFromThenTo

toList :: G.Vector v a => Vec n v a -> [a]
{-# INLINE toList #-}
toList (Vec vs) = G.toList vs

fromList :: G.Vector v a => [a] -> (forall n. Vec n v a -> r) -> r
{-# INLINE fromList #-}
fromList xs f = f (Vec (G.fromList xs))

stream :: G.Vector v a => Vec n v a -> Stream a
{-# INLINE stream #-}
stream (Vec vs) = G.stream vs

unstream :: G.Vector v a => Stream a -> (forall n. Vec n v a -> r) -> r
{-# INLINE unstream #-}
unstream s f = f (Vec (G.unstream s))

streamR :: G.Vector v a => Vec n v a -> Stream a
{-# INLINE streamR #-}
streamR (Vec vs) = G.streamR vs

unstreamR :: G.Vector v a => Stream a -> (forall n. Vec n v a -> r) -> r
{-# INLINE unstreamR #-}
unstreamR s f = f (Vec (G.unstreamR s))

new :: G.Vector v a => New a -> (forall n. Vec n v a -> r) -> r
{-# INLINE new #-}
new n f = f (Vec (G.new n))

allFin :: forall n v. (Nat n, G.Vector v (Fin n)) => n -> Vec n v (Fin n)
{-# INLINE allFin #-}
allFin n = Vec (G.generate (natToInt n) Fin)

indexed :: (G.Vector v a, G.Vector v (Fin n, a)) => Vec n v a -> Vec n v (Fin n, a)
{-# INLINE indexed #-}
indexed = imap (,)