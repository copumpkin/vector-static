{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, TypeFamilies, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances #-}
module Data.Vector.Generic.Static where

import Prelude hiding (succ, replicate, map)
import qualified Data.Vector.Generic as G
import Unsafe.Coerce

import Data.Nat
import Data.Fin

import Data.Vector.Fusion.Stream (Stream)
import Data.Vector.Generic.New (New)

newtype Vec n v a = Vec { unVec :: v a }
  deriving (Show, Eq)

-- length
-- null

empty :: G.Vector v a => Vec Z v a
empty = Vec G.empty

singleton :: G.Vector v a => a -> Vec (S Z) v a
singleton = Vec . G.singleton

cons :: G.Vector v a => a -> Vec n v a -> Vec (S n) v a
cons x (Vec xs) = Vec (G.cons x xs)

snoc :: G.Vector v a => Vec n v a -> a -> Vec (S n) v a
snoc (Vec xs) x = Vec (G.snoc xs x)

replicate :: forall a n v. (Nat n, G.Vector v a) => a -> Vec n v a
replicate = Vec . G.replicate (natToInt (witnessNat :: n))

generate :: forall n v a. (Nat n, G.Vector v a) =>  (Fin n -> a) -> Vec n v a
generate f = Vec (G.generate (natToInt (witnessNat :: n)) (f . Fin))

(++) :: G.Vector v a => Vec m v a -> Vec n v a -> Vec (m :+: n) v a
Vec ms ++ Vec ns = Vec (ms G.++ ns)

copy :: G.Vector v a => Vec n v a -> Vec n v a
copy (Vec vs) = Vec (G.copy vs)

(!) :: G.Vector v a => Vec n v a -> Fin n -> a
Vec vs ! Fin i = G.unsafeIndex vs i

head :: G.Vector v a => Vec (S n) v a -> a
head (Vec vs) = G.unsafeHead vs

last :: G.Vector v a => Vec (S n) v a -> a
last (Vec vs) = G.unsafeLast vs

-- indexM
-- headM
-- lastM

slice :: (G.Vector v a, Nat k) => Fin n -> k -> Vec (n :+: k) v a -> Vec k v a
slice (Fin i) k (Vec vs) = Vec (G.unsafeSlice i (natToInt k) vs)

init :: G.Vector v a => Vec (S n) v a -> Vec n v a
init (Vec vs) = Vec (G.unsafeInit vs)

tail :: G.Vector v a => Vec (S n) v a -> Vec n v a
tail (Vec vs) = Vec (G.unsafeTail vs)

take :: (G.Vector v a, Nat k) => k -> Vec (n :+: k) v a -> Vec k v a
take k (Vec vs) = Vec (G.take (natToInt k) vs)

drop :: (G.Vector v a, Nat k) => k -> Vec (n :+: k) v a -> Vec n v a
drop k (Vec vs) = Vec (G.drop (natToInt k) vs)

-- accum
-- accumulate
-- accumulate_
-- (//)
-- update
-- update_

backpermute :: (G.Vector v a, G.Vector v Int) => Vec m v a -> Vec n v (Fin m) -> Vec n v a
backpermute (Vec vs) (Vec is) = Vec (G.unsafeBackpermute vs (unsafeCoerce is))

reverse :: G.Vector v a => Vec n v a -> Vec n v a
reverse (Vec vs) = Vec (G.reverse vs)


map :: (G.Vector v a, G.Vector v b) => (a -> b) -> Vec n v a -> Vec n v b
map f (Vec vs) = Vec (G.map f vs)

imap :: (G.Vector v a, G.Vector v b) => (Fin n -> a -> b) -> Vec n v a -> Vec n v b
imap f (Vec vs) = Vec (G.imap (f . Fin) vs)

concatMap :: (G.Vector v a, G.Vector v b) => (a -> Vec n v b) -> Vec m v a -> Vec (m :*: n) v b
concatMap f (Vec as) = Vec (G.concatMap (unVec . f) as)


zipWith :: (G.Vector v a, G.Vector v b, G.Vector v c) => (a -> b -> c) -> Vec n v a -> Vec n v b -> Vec n v c
zipWith f (Vec as) (Vec bs) = Vec (G.zipWith f as bs)

zipWith3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d) => (a -> b -> c -> d) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d
zipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.zipWith3 f as bs cs)

zipWith4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e) => (a -> b -> c -> d -> e) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e
zipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.zipWith4 f as bs cs ds)

zipWith5 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v f) => (a -> b -> c -> d -> e -> f) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v f
zipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.zipWith5 f as bs cs ds es)

zipWith6 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v f, G.Vector v g) => (a -> b -> c -> d -> e -> f -> g) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v f -> Vec n v g
zipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.zipWith6 f as bs cs ds es fs)


izipWith :: (G.Vector v a, G.Vector v b, G.Vector v c) => (Fin n -> a -> b -> c) -> Vec n v a -> Vec n v b -> Vec n v c
izipWith f (Vec as) (Vec bs) = Vec (G.izipWith (f . Fin) as bs)

izipWith3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d) => (Fin n -> a -> b -> c -> d) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d
izipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.izipWith3 (f . Fin) as bs cs)

izipWith4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e) => (Fin n -> a -> b -> c -> d -> e) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e
izipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.izipWith4 (f . Fin) as bs cs ds)

izipWith5 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v f) => (Fin n -> a -> b -> c -> d -> e -> f) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v f
izipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.izipWith5 (f . Fin) as bs cs ds es)

izipWith6 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v f, G.Vector v g) => (Fin n -> a -> b -> c -> d -> e -> f -> g) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v f -> Vec n v g
izipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.izipWith6 (f . Fin) as bs cs ds es fs)


zip :: (G.Vector v a, G.Vector v b, G.Vector v (a, b)) => Vec n v a -> Vec n v b -> Vec n v (a, b)
zip (Vec as) (Vec bs) = Vec (G.zip as bs)
   
zip3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v (a, b, c)) => Vec n v a -> Vec n v b -> Vec n v c -> Vec n v (a, b, c)
zip3 (Vec as) (Vec bs) (Vec cs) = Vec (G.zip3 as bs cs)
   
zip4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v (a, b, c, d)) => Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v (a, b, c, d)
zip4 (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.zip4 as bs cs ds)
   
zip5 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v (a, b, c, d, e)) => Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v (a, b, c, d, e)
zip5 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.zip5 as bs cs ds es)
   
zip6 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v f, G.Vector v (a, b, c, d, e, f)) => Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v f -> Vec n v (a, b, c, d, e, f)
zip6 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.zip6 as bs cs ds es fs)


unzip :: (G.Vector v a, G.Vector v b, G.Vector v (a, b)) => Vec n v (a, b) -> (Vec n v a, Vec n v b)
unzip (Vec vs) = (Vec as, Vec bs)
  where (as, bs) = G.unzip vs

unzip3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v (a, b, c)) => Vec n v (a, b, c) -> (Vec n v a, Vec n v b, Vec n v c)
unzip3 (Vec vs) = (Vec as, Vec bs, Vec cs)
  where (as, bs, cs) = G.unzip3 vs

unzip4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v (a, b, c, d)) => Vec n v (a, b, c, d) -> (Vec n v a, Vec n v b, Vec n v c, Vec n v d)
unzip4 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds)
  where (as, bs, cs, ds) = G.unzip4 vs

unzip5 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v (a, b, c, d, e)) => Vec n v (a, b, c, d, e) -> (Vec n v a, Vec n v b, Vec n v c, Vec n v d, Vec n v e)
unzip5 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds, Vec es)
  where (as, bs, cs, ds, es) = G.unzip5 vs

unzip6 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e, G.Vector v f, G.Vector v (a, b, c, d, e, f)) => Vec n v (a, b, c, d, e, f) -> (Vec n v a, Vec n v b, Vec n v c, Vec n v d, Vec n v e, Vec n v f)
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
elem x (Vec vs) = G.elem x vs

notElem :: (G.Vector v a, Eq a) => a -> Vec n v a -> Bool
notElem x (Vec vs) = G.notElem x vs

find :: (G.Vector v a, Eq a) => (a -> Bool) -> Vec n v a -> Maybe a
find p (Vec vs) = G.find p vs

findIndex :: G.Vector v a => (a -> Bool) -> Vec n v a -> Maybe (Fin n)
findIndex p (Vec vs) = fmap Fin $ G.findIndex p vs

findIndices :: (G.Vector v a, G.Vector v Int, G.Vector v (Fin n)) => (a -> Bool) -> Vec n v a -> Vec m v (Fin n) -- should we return proof that m <= n?
findIndices p (Vec vs) = Vec (G.map Fin $ G.findIndices p vs)

elemIndex :: G.Vector v a => Eq a => a -> Vec n v a -> Maybe (Fin n)
elemIndex x (Vec vs) = fmap Fin $ G.elemIndex x vs

elemIndices :: (G.Vector v a, G.Vector v Int, G.Vector v (Fin n)) => Eq a => a -> Vec n v a -> Vec m v (Fin n)
elemIndices x (Vec vs) = Vec (G.map Fin $ G.elemIndices x vs)

foldl :: G.Vector v b => (a -> b -> a) -> a -> Vec n v b -> a
foldl f z (Vec vs) = G.foldl f z vs

foldl1 :: G.Vector v a => (a -> a -> a) -> Vec (S n) v a -> a
foldl1 f (Vec vs) = G.foldl1 f vs

foldl' :: G.Vector v b => (a -> b -> a) -> a -> Vec n v b -> a
foldl' f z (Vec vs) = G.foldl' f z vs

foldl1' :: G.Vector v a => (a -> a -> a) -> Vec (S n) v a -> a
foldl1' f (Vec vs) = G.foldl1' f vs

foldr :: G.Vector v a => (a -> b -> b) -> b -> Vec n v a -> b
foldr f z (Vec vs) = G.foldr f z vs

foldr1 :: G.Vector v a => (a -> a -> a) -> Vec (S n) v a -> a
foldr1 f (Vec vs) = G.foldr1 f vs

foldr' :: G.Vector v a => (a -> b -> b) -> b -> Vec n v a -> b
foldr' f z (Vec vs) = G.foldr' f z vs

foldr1' :: G.Vector v a => (a -> a -> a) -> Vec (S n) v a -> a
foldr1' f (Vec vs) = G.foldr1' f vs

ifoldl :: G.Vector v b => (a -> Fin n -> b -> a) -> a -> Vec n v b -> a
ifoldl f z (Vec vs) = G.ifoldl (\a b -> f a (Fin b)) z vs

ifoldl' :: G.Vector v b => (a -> Fin n -> b -> a) -> a -> Vec n v b -> a
ifoldl' f z (Vec vs) = G.ifoldl' (\a b -> f a (Fin b)) z vs

ifoldr :: G.Vector v a => (Fin n -> a -> b -> b) -> b -> Vec n v a -> b
ifoldr f z (Vec vs) = G.ifoldr (f . Fin) z vs

ifoldr' :: G.Vector v a => (Fin n -> a -> b -> b) -> b -> Vec n v a -> b
ifoldr' f z (Vec vs) = G.ifoldr' (f . Fin) z vs

all :: G.Vector v a => (a -> Bool) -> Vec n v a -> Bool
all p (Vec vs) = G.all p vs

any :: G.Vector v a => (a -> Bool) -> Vec n v a -> Bool
any p (Vec vs) = G.any p vs

and :: G.Vector v Bool => Vec n v Bool -> Bool
and (Vec vs) = G.and vs

or :: G.Vector v Bool => Vec n v Bool -> Bool
or (Vec vs) = G.or vs

sum :: (G.Vector v a, Num a) => Vec n v a -> a
sum (Vec vs) = G.sum vs

product :: (G.Vector v a, Num a) => Vec n v a -> a
product (Vec vs) = G.product vs

minimum :: (Ord a, G.Vector v a) => Vec (S n) v a -> a
minimum (Vec vs) = G.minimum vs

minimumBy :: G.Vector v a => (a -> a -> Ordering) -> Vec (S n) v a -> a
minimumBy c (Vec vs) = G.minimumBy c vs

minIndex :: (Ord a, G.Vector v a) => Vec (S n) v a -> Fin (S n)
minIndex (Vec vs) = Fin (G.minIndex vs)

minIndexBy :: G.Vector v a => (a -> a -> Ordering) -> Vec (S n) v a -> Fin (S n)
minIndexBy c (Vec vs) = Fin (G.minIndexBy c vs)

maximum :: (Ord a, G.Vector v a) => Vec (S n) v a -> a
maximum (Vec vs) = G.maximum vs

maximumBy :: G.Vector v a => (a -> a -> Ordering) -> Vec (S n) v a -> a
maximumBy c (Vec vs) = G.maximumBy c vs

maxIndex :: (Ord a, G.Vector v a) => Vec (S n) v a -> Fin (S n)
maxIndex (Vec vs) = Fin (G.maxIndex vs)

maxIndexBy :: G.Vector v a => (a -> a -> Ordering) -> Vec (S n) v a -> Fin (S n)
maxIndexBy c (Vec vs) = Fin (G.maxIndexBy c vs)

unfoldr :: G.Vector v a => (b -> Maybe (a, b)) -> b -> (forall n. Vec n v a -> r) -> r
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

enumFromN :: forall v a n. (G.Vector v a, Num a, Nat n) => a -> Vec n v a
enumFromN x = Vec (G.enumFromN x (natToInt (witnessNat :: n)))

enumFromStepN :: forall v a n. (G.Vector v a, Num a, Nat n) => a -> a -> Vec n v a
enumFromStepN x x1 = Vec (G.enumFromStepN x x1 (natToInt (witnessNat :: n)))

-- enumFromTo
-- enumFromThenTo

toList :: G.Vector v a => Vec n v a -> [a]
toList (Vec vs) = G.toList vs

fromList :: G.Vector v a => [a] -> (forall n. Vec n v a -> r) -> r
fromList xs f = f (Vec (G.fromList xs))

stream :: G.Vector v a => Vec n v a -> Stream a
stream (Vec vs) = G.stream vs

unstream :: G.Vector v a => Stream a -> (forall n. Vec n v a -> r) -> r
unstream s f = f (Vec (G.unstream s))

streamR :: G.Vector v a => Vec n v a -> Stream a
streamR (Vec vs) = G.streamR vs

unstreamR :: G.Vector v a => Stream a -> (forall n. Vec n v a -> r) -> r
unstreamR s f = f (Vec (G.unstreamR s))

new :: G.Vector v a => New a -> (forall n. Vec n v a -> r) -> r
new n f = f (Vec (G.new n))

allFin :: forall n v. (Nat n, G.Vector v (Fin n)) => Vec n v (Fin n)
allFin = Vec (G.generate (natToInt (witnessNat :: n)) Fin)
