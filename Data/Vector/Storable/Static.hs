{-# LANGUAGE TypeOperators, RankNTypes #-}
module Data.Vector.Storable.Static where

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Generic.Static as G

import Data.Vector.Fusion.Stream
import Data.Vector.Generic.New

import Data.Nat
import Data.Fin

newtype Vec n a = Vec { unVec :: G.Vec n V.Vector a }

length :: (V.Storable a, Nat n) => Vec n a -> n
length = G.length . unVec

-- null

empty :: V.Storable a => Vec Z a
empty = Vec G.empty

singleton :: V.Storable a => a -> Vec (S Z) a
singleton = Vec . G.singleton

cons :: V.Storable a => a -> Vec n a -> Vec (S n) a
cons x (Vec xs) = Vec (G.cons x xs)

snoc :: V.Storable a => Vec n a -> a -> Vec (S n) a
snoc (Vec xs) x = Vec (G.snoc xs x)

replicate :: (V.Storable a, Nat n) => n -> a -> Vec n a
replicate n = Vec . G.replicate n

generate :: (V.Storable a, Nat n) => n -> (Fin n -> a) -> Vec n a
generate n f = Vec (G.generate n f)

(++) :: V.Storable a => Vec m a -> Vec n a -> Vec (m :+: n) a
Vec ms ++ Vec ns = Vec (ms G.++ ns)

copy :: V.Storable a => Vec n a -> Vec n a
copy (Vec vs) = Vec (G.copy vs)

(!) :: V.Storable a => Vec n a -> Fin n -> a
Vec vs ! i = vs G.! i

head :: V.Storable a => Vec (S n) a -> a
head (Vec vs) = G.head vs

last :: V.Storable a => Vec (S n) a -> a
last (Vec vs) = G.last vs

-- indexM
-- headM
-- lastM

-- slice

init :: V.Storable a => Vec (S n) a -> Vec n a
init (Vec vs) = Vec (G.init vs)

tail :: V.Storable a => Vec (S n) a -> Vec n a
tail (Vec vs) = Vec (G.tail vs)

-- take
-- drop

-- accum
-- accumulate
-- accumulate_
-- (//)
-- update
-- update_

backpermute :: V.Storable a => Vec m a -> Vec n (Fin m) -> Vec n a
backpermute (Vec vs) (Vec is) = Vec (G.backpermute vs is)

reverse :: V.Storable a => Vec n a -> Vec n a
reverse (Vec vs) = Vec (G.reverse vs)

map :: (V.Storable a, V.Storable b) => (a -> b) -> Vec n a -> Vec n b
map f (Vec vs) = Vec (G.map f vs)

imap :: (V.Storable a, V.Storable b) => (Fin n -> a -> b) -> Vec n a -> Vec n b
imap f (Vec vs) = Vec (G.imap f vs)

concatMap :: (V.Storable a, V.Storable b) => (a -> Vec n b) -> Vec m a -> Vec (m :*: n) b
concatMap f (Vec as) = Vec (G.concatMap (unVec . f) as)

zipWith :: (V.Storable a, V.Storable b, V.Storable c) => (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith f (Vec as) (Vec bs) = Vec (G.zipWith f as bs)

zipWith3 :: (V.Storable a, V.Storable b, V.Storable c, V.Storable d) => (a -> b -> c -> d) -> Vec n a -> Vec n b -> Vec n c -> Vec n d
zipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.zipWith3 f as bs cs)

zipWith4 :: (V.Storable a, V.Storable b, V.Storable c, V.Storable d, V.Storable e) => (a -> b -> c -> d -> e) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e
zipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.zipWith4 f as bs cs ds)

zipWith5 :: (V.Storable a, V.Storable b, V.Storable c, V.Storable d, V.Storable e, V.Storable f) => (a -> b -> c -> d -> e -> f) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f
zipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.zipWith5 f as bs cs ds es)

zipWith6 :: (V.Storable a, V.Storable b, V.Storable c, V.Storable d, V.Storable e, V.Storable f, V.Storable g) => (a -> b -> c -> d -> e -> f -> g) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f -> Vec n g
zipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.zipWith6 f as bs cs ds es fs)


izipWith :: (V.Storable a, V.Storable b, V.Storable c) => (Fin n -> a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
izipWith f (Vec as) (Vec bs) = Vec (G.izipWith f as bs)

izipWith3 :: (V.Storable a, V.Storable b, V.Storable c, V.Storable d) => (Fin n -> a -> b -> c -> d) -> Vec n a -> Vec n b -> Vec n c -> Vec n d
izipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.izipWith3 f as bs cs)

izipWith4 :: (V.Storable a, V.Storable b, V.Storable c, V.Storable d, V.Storable e) => (Fin n -> a -> b -> c -> d -> e) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e
izipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.izipWith4 f as bs cs ds)

izipWith5 :: (V.Storable a, V.Storable b, V.Storable c, V.Storable d, V.Storable e, V.Storable f) => (Fin n -> a -> b -> c -> d -> e -> f) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f
izipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.izipWith5 f as bs cs ds es)

izipWith6 :: (V.Storable a, V.Storable b, V.Storable c, V.Storable d, V.Storable e, V.Storable f, V.Storable g) => (Fin n -> a -> b -> c -> d -> e -> f -> g) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f -> Vec n g
izipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.izipWith6 f as bs cs ds es fs)


-- filter
-- ifilter
-- takeWhile
-- dropWhile
-- partition
-- unstablePartition
-- span
-- break

elem :: (V.Storable a, Eq a) => a -> Vec n a -> Bool
elem x (Vec vs) = G.elem x vs

notElem :: (V.Storable a, Eq a) => a -> Vec n a -> Bool
notElem x (Vec vs) = G.notElem x vs

find :: (V.Storable a, Eq a) => (a -> Bool) -> Vec n a -> Maybe a
find p (Vec vs) = G.find p vs

findIndex :: V.Storable a => (a -> Bool) -> Vec n a -> Maybe (Fin n)
findIndex p (Vec vs) = G.findIndex p vs

--findIndices :: V.Storable a => (a -> Bool) -> Vec n a -> Vec m (Fin n)
--findIndices p (Vec vs) = Vec (G.findIndices p vs)

elemIndex :: (V.Storable a, Eq a) => a -> Vec n a -> Maybe (Fin n)
elemIndex x (Vec vs) = G.elemIndex x vs

--elemIndices :: (V.Storable a, Eq a) => a -> Vec n a -> Vec m (Fin n)
--elemIndices x (Vec vs) = Vec (G.elemIndices x vs)


foldl :: V.Storable b => (a -> b -> a) -> a -> Vec n b -> a
foldl f z (Vec vs) = G.foldl f z vs

foldl1 :: V.Storable a => (a -> a -> a) -> Vec (S n) a -> a
foldl1 f (Vec vs) = G.foldl1 f vs

foldl' :: V.Storable b => (a -> b -> a) -> a -> Vec n b -> a
foldl' f z (Vec vs) = G.foldl' f z vs

foldl1' :: V.Storable a => (a -> a -> a) -> Vec (S n) a -> a
foldl1' f (Vec vs) = G.foldl1' f vs

foldr :: V.Storable a => (a -> b -> b) -> b -> Vec n a -> b
foldr f z (Vec vs) = G.foldr f z vs

foldr1 :: V.Storable a => (a -> a -> a) -> Vec (S n) a -> a
foldr1 f (Vec vs) = G.foldr1 f vs

foldr' :: V.Storable a => (a -> b -> b) -> b -> Vec n a -> b
foldr' f z (Vec vs) = G.foldr' f z vs

foldr1' :: V.Storable a => (a -> a -> a) -> Vec (S n) a -> a
foldr1' f (Vec vs) = G.foldr1' f vs

ifoldl :: V.Storable b => (a -> Fin n -> b -> a) -> a -> Vec n b -> a
ifoldl f z (Vec vs) = G.ifoldl f z vs

ifoldl' :: V.Storable b => (a -> Fin n -> b -> a) -> a -> Vec n b -> a
ifoldl' f z (Vec vs) = G.ifoldl' f z vs

ifoldr :: V.Storable a => (Fin n -> a -> b -> b) -> b -> Vec n a -> b
ifoldr f z (Vec vs) = G.ifoldr f z vs

ifoldr' :: V.Storable a => (Fin n -> a -> b -> b) -> b -> Vec n a -> b
ifoldr' f z (Vec vs) = G.ifoldr' f z vs

all :: V.Storable a => (a -> Bool) -> Vec n a -> Bool
all p (Vec vs) = G.all p vs

any :: V.Storable a => (a -> Bool) -> Vec n a -> Bool
any p (Vec vs) = G.any p vs

and :: Vec n Bool -> Bool
and (Vec vs) = G.and vs

or :: Vec n Bool -> Bool
or (Vec vs) = G.or vs

sum :: (V.Storable a, Num a) => Vec n a -> a
sum (Vec vs) = G.sum vs

product :: (V.Storable a, Num a) => Vec n a -> a
product (Vec vs) = G.product vs


minimum :: (V.Storable a, Ord a) => Vec (S n) a -> a
minimum (Vec vs) = G.minimum vs

minimumBy :: V.Storable a => (a -> a -> Ordering) -> Vec (S n) a -> a
minimumBy c (Vec vs) = G.minimumBy c vs

minIndex :: (V.Storable a, Ord a) => Vec (S n) a -> Fin (S n)
minIndex (Vec vs) = G.minIndex vs

minIndexBy :: V.Storable a => (a -> a -> Ordering) -> Vec (S n) a -> Fin (S n)
minIndexBy c (Vec vs) = G.minIndexBy c vs

maximum :: (V.Storable a, Ord a) => Vec (S n) a -> a
maximum (Vec vs) = G.maximum vs

maximumBy :: V.Storable a => (a -> a -> Ordering) -> Vec (S n) a -> a
maximumBy c (Vec vs) = G.maximumBy c vs

maxIndex :: (V.Storable a, Ord a) => Vec (S n) a -> Fin (S n)
maxIndex (Vec vs) = G.maxIndex vs

maxIndexBy :: V.Storable a => (a -> a -> Ordering) -> Vec (S n) a -> Fin (S n)
maxIndexBy c (Vec vs) = G.maxIndexBy c vs

unfoldr :: V.Storable a => (b -> Maybe (a, b)) -> b -> (forall n. Vec n a -> r) -> r
unfoldr f x c = G.unfoldr f x (c . Vec)

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

enumFromN :: forall a n. (V.Storable a, Num a, Nat n) => a -> n -> Vec n a
enumFromN x n = Vec (G.enumFromN x n)

enumFromStepN :: forall a n. (V.Storable a, Num a, Nat n) => a -> a -> n -> Vec n a
enumFromStepN x x1 n = Vec (G.enumFromStepN x x1 n)

-- enumFromTo
-- enumFromThenTo

toList :: V.Storable a => Vec n a -> [a]
toList (Vec vs) = G.toList vs

fromList :: V.Storable a => [a] -> (forall n. Vec n a -> r) -> r
fromList xs f = G.fromList xs (f . Vec)

stream :: V.Storable a => Vec n a -> Stream a
stream (Vec vs) = G.stream vs

unstream :: V.Storable a => Stream a -> (forall n. Vec n a -> r) -> r
unstream s f = G.unstream s (f . Vec)

streamR :: V.Storable a => Vec n a -> Stream a
streamR (Vec vs) = G.streamR vs

unstreamR :: V.Storable a => Stream a -> (forall n. Vec n a -> r) -> r
unstreamR s f = G.unstreamR s (f . Vec)

new :: V.Storable a => New a -> (forall n. Vec n a -> r) -> r
new n f = G.new n (f . Vec)

--allFin :: Nat n => Vec n (Fin n)
--allFin = Vec G.allFin

