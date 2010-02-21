{-# LANGUAGE TypeOperators, RankNTypes #-}
module Data.Vector.Static where

import qualified Data.Vector as V
import qualified Data.Vector.Generic.Static as G

import Data.Vector.Fusion.Stream
import Data.Vector.Generic.New

import Data.Nat
import Data.Fin

import Control.Arrow

newtype Vec n a = Vec { unVec :: G.Vec n V.Vector a }

-- length
-- null

empty :: Vec Z a
empty = Vec G.empty

singleton :: a -> Vec (S Z) a
singleton = Vec . G.singleton

cons :: a -> Vec n a -> Vec (S n) a
cons x (Vec xs) = Vec (G.cons x xs)

snoc :: Vec n a -> a -> Vec (S n) a
snoc (Vec xs) x = Vec (G.snoc xs x)

replicate :: Nat n => a -> Vec n a
replicate = Vec . G.replicate

generate :: Nat n => (Fin n -> a) -> Vec n a
generate f = Vec (G.generate f)

(++) :: Vec m a -> Vec n a -> Vec (m :+: n) a
Vec ms ++ Vec ns = Vec (ms G.++ ns)

copy :: Vec n a -> Vec n a
copy (Vec vs) = Vec (G.copy vs)

(!) :: Vec n a -> Fin n -> a
Vec vs ! i = vs G.! i

head :: Vec (S n) a -> a
head (Vec vs) = G.head vs

last :: Vec (S n) a -> a
last (Vec vs) = G.last vs

-- indexM
-- headM
-- lastM

-- slice

init :: Vec (S n) a -> Vec n a
init (Vec vs) = Vec (G.init vs)

tail :: Vec (S n) a -> Vec n a
tail (Vec vs) = Vec (G.tail vs)

-- take
-- drop

-- accum
-- accumulate
-- accumulate_
-- (//)
-- update
-- update_

backpermute :: Vec m a -> Vec n (Fin m) -> Vec n a
backpermute (Vec vs) (Vec is) = Vec (G.backpermute vs is)

reverse :: Vec n a -> Vec n a
reverse (Vec vs) = Vec (G.reverse vs)

map :: (a -> b) -> Vec n a -> Vec n b
map f (Vec vs) = Vec (G.map f vs)

imap :: (Fin n -> a -> b) -> Vec n a -> Vec n b
imap f (Vec vs) = Vec (G.imap f vs)

concatMap :: (a -> Vec n b) -> Vec m a -> Vec (m :*: n) b
concatMap f (Vec as) = Vec (G.concatMap (unVec . f) as)

zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith f (Vec as) (Vec bs) = Vec (G.zipWith f as bs)

zipWith3 :: (a -> b -> c -> d) -> Vec n a -> Vec n b -> Vec n c -> Vec n d
zipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.zipWith3 f as bs cs)

zipWith4 :: (a -> b -> c -> d -> e) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e
zipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.zipWith4 f as bs cs ds)

zipWith5 :: (a -> b -> c -> d -> e -> f) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f
zipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.zipWith5 f as bs cs ds es)

zipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f -> Vec n g
zipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.zipWith6 f as bs cs ds es fs)


izipWith :: (Fin n -> a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
izipWith f (Vec as) (Vec bs) = Vec (G.izipWith f as bs)

izipWith3 :: (Fin n -> a -> b -> c -> d) -> Vec n a -> Vec n b -> Vec n c -> Vec n d
izipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.izipWith3 f as bs cs)

izipWith4 :: (Fin n -> a -> b -> c -> d -> e) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e
izipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.izipWith4 f as bs cs ds)

izipWith5 :: (Fin n -> a -> b -> c -> d -> e -> f) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f
izipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.izipWith5 f as bs cs ds es)

izipWith6 :: (Fin n -> a -> b -> c -> d -> e -> f -> g) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f -> Vec n g
izipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.izipWith6 f as bs cs ds es fs)


zip :: Vec n a -> Vec n b -> Vec n (a, b)
zip (Vec as) (Vec bs) = Vec (G.zip as bs)

zip3 :: Vec n a -> Vec n b -> Vec n c -> Vec n (a, b, c)
zip3 (Vec as) (Vec bs) (Vec cs) = Vec (G.zip3 as bs cs)
   
zip4 :: Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n (a, b, c, d)
zip4 (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.zip4 as bs cs ds)
   
zip5 :: Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n (a, b, c, d, e)
zip5 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.zip5 as bs cs ds es)
   
zip6 :: Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f -> Vec n (a, b, c, d, e, f)
zip6 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.zip6 as bs cs ds es fs)



unzip :: Vec n (a, b) -> (Vec n a, Vec n b)
unzip (Vec abs) = Vec *** Vec $ G.unzip abs

unzip3 :: Vec n (a, b, c) -> (Vec n a, Vec n b, Vec n c)
unzip3 (Vec vs) = (Vec as, Vec bs, Vec cs)
  where (as, bs, cs) = G.unzip3 vs

unzip4 :: Vec n (a, b, c, d) -> (Vec n a, Vec n b, Vec n c, Vec n d)
unzip4 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds)
  where (as, bs, cs, ds) = G.unzip4 vs

unzip5 :: Vec n (a, b, c, d, e) -> (Vec n a, Vec n b, Vec n c, Vec n d, Vec n e)
unzip5 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds, Vec es)
  where (as, bs, cs, ds, es) = G.unzip5 vs

unzip6 :: Vec n (a, b, c, d, e, f) -> (Vec n a, Vec n b, Vec n c, Vec n d, Vec n e, Vec n f)
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

elem :: Eq a => a -> Vec n a -> Bool
elem x (Vec vs) = G.elem x vs

notElem :: Eq a => a -> Vec n a -> Bool
notElem x (Vec vs) = G.notElem x vs

find :: Eq a => (a -> Bool) -> Vec n a -> Maybe a
find p (Vec vs) = G.find p vs

findIndex :: (a -> Bool) -> Vec n a -> Maybe (Fin n)
findIndex p (Vec vs) = G.findIndex p vs

findIndices :: (a -> Bool) -> Vec n a -> Vec m (Fin n)
findIndices p (Vec vs) = Vec (G.findIndices p vs)

elemIndex :: Eq a => a -> Vec n a -> Maybe (Fin n)
elemIndex x (Vec vs) = G.elemIndex x vs

elemIndices :: Eq a => a -> Vec n a -> Vec m (Fin n)
elemIndices x (Vec vs) = Vec (G.elemIndices x vs)


foldl :: (a -> b -> a) -> a -> Vec n b -> a
foldl f z (Vec vs) = G.foldl f z vs

foldl1 :: (a -> a -> a) -> Vec (S n) a -> a
foldl1 f (Vec vs) = G.foldl1 f vs

foldl' :: (a -> b -> a) -> a -> Vec n b -> a
foldl' f z (Vec vs) = G.foldl' f z vs

foldl1' :: (a -> a -> a) -> Vec (S n) a -> a
foldl1' f (Vec vs) = G.foldl1' f vs

foldr :: (a -> b -> b) -> b -> Vec n a -> b
foldr f z (Vec vs) = G.foldr f z vs

foldr1 :: (a -> a -> a) -> Vec (S n) a -> a
foldr1 f (Vec vs) = G.foldr1 f vs

foldr' :: (a -> b -> b) -> b -> Vec n a -> b
foldr' f z (Vec vs) = G.foldr' f z vs

foldr1' :: (a -> a -> a) -> Vec (S n) a -> a
foldr1' f (Vec vs) = G.foldr1' f vs

ifoldl :: (a -> Fin n -> b -> a) -> a -> Vec n b -> a
ifoldl f z (Vec vs) = G.ifoldl f z vs

ifoldl' :: (a -> Fin n -> b -> a) -> a -> Vec n b -> a
ifoldl' f z (Vec vs) = G.ifoldl' f z vs

ifoldr :: (Fin n -> a -> b -> b) -> b -> Vec n a -> b
ifoldr f z (Vec vs) = G.ifoldr f z vs

ifoldr' :: (Fin n -> a -> b -> b) -> b -> Vec n a -> b
ifoldr' f z (Vec vs) = G.ifoldr' f z vs

all :: (a -> Bool) -> Vec n a -> Bool
all p (Vec vs) = G.all p vs

any :: (a -> Bool) -> Vec n a -> Bool
any p (Vec vs) = G.any p vs

and :: Vec n Bool -> Bool
and (Vec vs) = G.and vs

or :: Vec n Bool -> Bool
or (Vec vs) = G.or vs

sum :: Num a => Vec n a -> a
sum (Vec vs) = G.sum vs

product :: Num a => Vec n a -> a
product (Vec vs) = G.product vs


minimum :: Ord a => Vec (S n) a -> a
minimum (Vec vs) = G.minimum vs

minimumBy :: (a -> a -> Ordering) -> Vec (S n) a -> a
minimumBy c (Vec vs) = G.minimumBy c vs

minIndex :: Ord a => Vec (S n) a -> Fin (S n)
minIndex (Vec vs) = G.minIndex vs

minIndexBy :: (a -> a -> Ordering) -> Vec (S n) a -> Fin (S n)
minIndexBy c (Vec vs) = G.minIndexBy c vs

maximum :: Ord a => Vec (S n) a -> a
maximum (Vec vs) = G.maximum vs

maximumBy :: (a -> a -> Ordering) -> Vec (S n) a -> a
maximumBy c (Vec vs) = G.maximumBy c vs

maxIndex :: Ord a => Vec (S n) a -> Fin (S n)
maxIndex (Vec vs) = G.maxIndex vs

maxIndexBy :: (a -> a -> Ordering) -> Vec (S n) a -> Fin (S n)
maxIndexBy c (Vec vs) = G.maxIndexBy c vs

unfoldr :: (b -> Maybe (a, b)) -> b -> (forall n. Vec n a -> r) -> r
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

enumFromN :: forall a n. (Num a, Nat n) => a -> Vec n a
enumFromN x = Vec (G.enumFromN x)

enumFromStepN :: forall a n. (Num a, Nat n) => a -> a -> Vec n a
enumFromStepN x x1 = Vec (G.enumFromStepN x x1)

-- enumFromTo
-- enumFromThenTo

toList :: Vec n a -> [a]
toList (Vec vs) = G.toList vs

fromList :: [a] -> (forall n. Vec n a -> r) -> r
fromList xs f = G.fromList xs (f . Vec)

stream :: Vec n a -> Stream a
stream (Vec vs) = G.stream vs

unstream :: Stream a -> (forall n. Vec n a -> r) -> r
unstream s f = G.unstream s (f . Vec)

streamR :: Vec n a -> Stream a
streamR (Vec vs) = G.streamR vs

unstreamR :: Stream a -> (forall n. Vec n a -> r) -> r
unstreamR s f = G.unstreamR s (f . Vec)

new :: New a -> (forall n. Vec n a -> r) -> r
new n f = G.new n (f . Vec)

allFin :: Nat n => Vec n (Fin n)
allFin = Vec G.allFin

