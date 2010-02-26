{-# LANGUAGE TypeOperators, RankNTypes, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Data.Vector.Static where

import Prelude hiding (map, replicate, zipWith, concatMap)

import Control.Applicative

import qualified Data.Vector as V
import qualified Data.Vector.Generic as OG
import qualified Data.Vector.Generic.Static as G

import Data.Vector.Fusion.Stream (Stream)
import Data.Vector.Generic.New (New)

import Data.Nat
import Data.Fin

newtype Vec n a = Vec { unVec :: G.Vec n V.Vector a }

instance Functor (Vec n) where
  fmap = map

-- The Applicative and Monad instances are slow because of witnessNat! avoid them!
instance Nat n => Applicative (Vec n) where
  pure = replicate witnessNat
  (<*>) = zipWith id
  
instance Nat n => Monad (Vec n) where
  return  = replicate witnessNat
  x >>= f = diagonal (fmap f x)

diagonal :: Nat n => Vec n (Vec n a) -> Vec n a
{-# INLINE diagonal #-}
diagonal vs = zipWith (!) vs (allFin witnessNat)

-- deriving instance OG.Vector V.Vector a => OG.Vector (Vec n) a :(

length :: Nat n => Vec n a -> n
{-# INLINE length #-}
length = G.length . unVec

-- null

empty :: Vec Z a
{-# INLINE empty #-}
empty = Vec G.empty

singleton :: a -> Vec (S Z) a
{-# INLINE singleton #-}
singleton = Vec . G.singleton

cons :: a -> Vec n a -> Vec (S n) a
{-# INLINE cons #-}
cons x (Vec xs) = Vec (G.cons x xs)

snoc :: Vec n a -> a -> Vec (S n) a
{-# INLINE snoc #-}
snoc (Vec xs) x = Vec (G.snoc xs x)

replicate :: Nat n => n -> a -> Vec n a
{-# INLINE replicate #-}
replicate n = Vec . G.replicate n

generate :: Nat n => n -> (Fin n -> a) -> Vec n a
{-# INLINE generate #-}
generate n f = Vec (G.generate n f)

(++) :: Vec m a -> Vec n a -> Vec (m :+: n) a
{-# INLINE (++) #-}
Vec ms ++ Vec ns = Vec (ms G.++ ns)

copy :: Vec n a -> Vec n a
{-# INLINE copy #-}
copy (Vec vs) = Vec (G.copy vs)

(!) :: Vec n a -> Fin n -> a
{-# INLINE (!) #-}
Vec vs ! i = vs G.! i

head :: Vec (S n) a -> a
{-# INLINE head #-}
head (Vec vs) = G.head vs

last :: Vec (S n) a -> a
{-# INLINE last #-}
last (Vec vs) = G.last vs

-- indexM
-- headM
-- lastM

-- slice

init :: Vec (S n) a -> Vec n a
{-# INLINE init #-}
init (Vec vs) = Vec (G.init vs)

tail :: Vec (S n) a -> Vec n a
{-# INLINE tail #-}
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
{-# INLINE backpermute #-}
backpermute (Vec vs) (Vec is) = Vec (G.backpermute vs is)

reverse :: Vec n a -> Vec n a
{-# INLINE reverse #-}
reverse (Vec vs) = Vec (G.reverse vs)

map :: (a -> b) -> Vec n a -> Vec n b
{-# INLINE map #-}
map f (Vec vs) = Vec (G.map f vs)

imap :: (Fin n -> a -> b) -> Vec n a -> Vec n b
{-# INLINE imap #-}
imap f (Vec vs) = Vec (G.imap f vs)

concatMap :: (a -> Vec n b) -> Vec m a -> Vec (m :*: n) b
{-# INLINE concatMap #-}
concatMap f (Vec as) = Vec (G.concatMap (unVec . f) as)

zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
{-# INLINE zipWith #-}
zipWith f (Vec as) (Vec bs) = Vec (G.zipWith f as bs)

zipWith3 :: (a -> b -> c -> d) -> Vec n a -> Vec n b -> Vec n c -> Vec n d
{-# INLINE zipWith3 #-}
zipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.zipWith3 f as bs cs)

zipWith4 :: (a -> b -> c -> d -> e) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e
{-# INLINE zipWith4 #-}
zipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.zipWith4 f as bs cs ds)

zipWith5 :: (a -> b -> c -> d -> e -> f) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f
{-# INLINE zipWith5 #-}
zipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.zipWith5 f as bs cs ds es)

zipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f -> Vec n g
{-# INLINE zipWith6 #-}
zipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.zipWith6 f as bs cs ds es fs)


izipWith :: (Fin n -> a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
{-# INLINE izipWith #-}
izipWith f (Vec as) (Vec bs) = Vec (G.izipWith f as bs)

izipWith3 :: (Fin n -> a -> b -> c -> d) -> Vec n a -> Vec n b -> Vec n c -> Vec n d
{-# INLINE izipWith3 #-}
izipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.izipWith3 f as bs cs)

izipWith4 :: (Fin n -> a -> b -> c -> d -> e) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e
{-# INLINE izipWith4 #-}
izipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.izipWith4 f as bs cs ds)

izipWith5 :: (Fin n -> a -> b -> c -> d -> e -> f) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f
{-# INLINE izipWith5 #-}
izipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.izipWith5 f as bs cs ds es)

izipWith6 :: (Fin n -> a -> b -> c -> d -> e -> f -> g) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f -> Vec n g
{-# INLINE izipWith6 #-}
izipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.izipWith6 f as bs cs ds es fs)


zip :: Vec n a -> Vec n b -> Vec n (a, b)
{-# INLINE zip #-}
zip (Vec as) (Vec bs) = Vec (G.zip as bs)

zip3 :: Vec n a -> Vec n b -> Vec n c -> Vec n (a, b, c)
{-# INLINE zip3 #-}
zip3 (Vec as) (Vec bs) (Vec cs) = Vec (G.zip3 as bs cs)
   
zip4 :: Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n (a, b, c, d)
{-# INLINE zip4 #-}
zip4 (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.zip4 as bs cs ds)
   
zip5 :: Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n (a, b, c, d, e)
{-# INLINE zip5 #-}
zip5 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.zip5 as bs cs ds es)
   
zip6 :: Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f -> Vec n (a, b, c, d, e, f)
{-# INLINE zip6 #-}
zip6 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.zip6 as bs cs ds es fs)



unzip :: Vec n (a, b) -> (Vec n a, Vec n b)
{-# INLINE unzip #-}
unzip (Vec vs) = (Vec as, Vec bs)
  where (as, bs) = G.unzip vs

unzip3 :: Vec n (a, b, c) -> (Vec n a, Vec n b, Vec n c)
{-# INLINE unzip3 #-}
unzip3 (Vec vs) = (Vec as, Vec bs, Vec cs)
  where (as, bs, cs) = G.unzip3 vs

unzip4 :: Vec n (a, b, c, d) -> (Vec n a, Vec n b, Vec n c, Vec n d)
{-# INLINE unzip4 #-}
unzip4 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds)
  where (as, bs, cs, ds) = G.unzip4 vs

unzip5 :: Vec n (a, b, c, d, e) -> (Vec n a, Vec n b, Vec n c, Vec n d, Vec n e)
{-# INLINE unzip5 #-}
unzip5 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds, Vec es)
  where (as, bs, cs, ds, es) = G.unzip5 vs

unzip6 :: Vec n (a, b, c, d, e, f) -> (Vec n a, Vec n b, Vec n c, Vec n d, Vec n e, Vec n f)
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

elem :: Eq a => a -> Vec n a -> Bool
{-# INLINE elem #-}
elem x (Vec vs) = G.elem x vs

notElem :: Eq a => a -> Vec n a -> Bool
{-# INLINE notElem #-}
notElem x (Vec vs) = G.notElem x vs

find :: Eq a => (a -> Bool) -> Vec n a -> Maybe a
{-# INLINE find #-}
find p (Vec vs) = G.find p vs

findIndex :: (a -> Bool) -> Vec n a -> Maybe (Fin n)
{-# INLINE findIndex #-}
findIndex p (Vec vs) = G.findIndex p vs

findIndices :: (a -> Bool) -> Vec n a -> Vec m (Fin n)
{-# INLINE findIndices #-}
findIndices p (Vec vs) = Vec (G.findIndices p vs)

elemIndex :: Eq a => a -> Vec n a -> Maybe (Fin n)
{-# INLINE elemIndex #-}
elemIndex x (Vec vs) = G.elemIndex x vs

elemIndices :: Eq a => a -> Vec n a -> Vec m (Fin n)
{-# INLINE elemIndices #-}
elemIndices x (Vec vs) = Vec (G.elemIndices x vs)


foldl :: (a -> b -> a) -> a -> Vec n b -> a
{-# INLINE foldl #-}
foldl f z (Vec vs) = G.foldl f z vs

foldl1 :: (a -> a -> a) -> Vec (S n) a -> a
{-# INLINE foldl1 #-}
foldl1 f (Vec vs) = G.foldl1 f vs

foldl' :: (a -> b -> a) -> a -> Vec n b -> a
foldl' f z (Vec vs) = G.foldl' f z vs

foldl1' :: (a -> a -> a) -> Vec (S n) a -> a
foldl1' f (Vec vs) = G.foldl1' f vs

foldr :: (a -> b -> b) -> b -> Vec n a -> b
{-# INLINE foldr #-}
foldr f z (Vec vs) = G.foldr f z vs

foldr1 :: (a -> a -> a) -> Vec (S n) a -> a
{-# INLINE foldr1 #-}
foldr1 f (Vec vs) = G.foldr1 f vs

foldr' :: (a -> b -> b) -> b -> Vec n a -> b
foldr' f z (Vec vs) = G.foldr' f z vs

foldr1' :: (a -> a -> a) -> Vec (S n) a -> a
foldr1' f (Vec vs) = G.foldr1' f vs

ifoldl :: (a -> Fin n -> b -> a) -> a -> Vec n b -> a
{-# INLINE ifoldl #-}
ifoldl f z (Vec vs) = G.ifoldl f z vs

ifoldl' :: (a -> Fin n -> b -> a) -> a -> Vec n b -> a
ifoldl' f z (Vec vs) = G.ifoldl' f z vs

ifoldr :: (Fin n -> a -> b -> b) -> b -> Vec n a -> b
{-# INLINE ifoldr #-}
ifoldr f z (Vec vs) = G.ifoldr f z vs

ifoldr' :: (Fin n -> a -> b -> b) -> b -> Vec n a -> b
ifoldr' f z (Vec vs) = G.ifoldr' f z vs

all :: (a -> Bool) -> Vec n a -> Bool
{-# INLINE all #-}
all p (Vec vs) = G.all p vs

any :: (a -> Bool) -> Vec n a -> Bool
{-# INLINE any #-}
any p (Vec vs) = G.any p vs

and :: Vec n Bool -> Bool
{-# INLINE and #-}
and (Vec vs) = G.and vs

or :: Vec n Bool -> Bool
{-# INLINE or #-}
or (Vec vs) = G.or vs

sum :: Num a => Vec n a -> a
{-# INLINE sum #-}
sum (Vec vs) = G.sum vs

product :: Num a => Vec n a -> a
{-# INLINE product #-}
product (Vec vs) = G.product vs


minimum :: Ord a => Vec (S n) a -> a
{-# INLINE minimum #-}
minimum (Vec vs) = G.minimum vs

minimumBy :: (a -> a -> Ordering) -> Vec (S n) a -> a
{-# INLINE minimumBy #-}
minimumBy c (Vec vs) = G.minimumBy c vs

minIndex :: Ord a => Vec (S n) a -> Fin (S n)
{-# INLINE minIndex #-}
minIndex (Vec vs) = G.minIndex vs

minIndexBy :: (a -> a -> Ordering) -> Vec (S n) a -> Fin (S n)
{-# INLINE minIndexBy #-}
minIndexBy c (Vec vs) = G.minIndexBy c vs

maximum :: Ord a => Vec (S n) a -> a
{-# INLINE maximum #-}
maximum (Vec vs) = G.maximum vs

maximumBy :: (a -> a -> Ordering) -> Vec (S n) a -> a
{-# INLINE maximumBy #-}
maximumBy c (Vec vs) = G.maximumBy c vs

maxIndex :: Ord a => Vec (S n) a -> Fin (S n)
{-# INLINE maxIndex #-}
maxIndex (Vec vs) = G.maxIndex vs

maxIndexBy :: (a -> a -> Ordering) -> Vec (S n) a -> Fin (S n)
{-# INLINE maxIndexBy #-}
maxIndexBy c (Vec vs) = G.maxIndexBy c vs

unfoldr :: (b -> Maybe (a, b)) -> b -> (forall n. Vec n a -> r) -> r
{-# INLINE unfoldr #-}
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

enumFromN :: forall a n. (Num a, Nat n) => a -> n -> Vec n a
{-# INLINE enumFromN #-}
enumFromN x n = Vec (G.enumFromN x n)

enumFromStepN :: forall a n. (Num a, Nat n) => a -> a -> n -> Vec n a
{-# INLINE enumFromStepN #-}
enumFromStepN x x1 n = Vec (G.enumFromStepN x x1 n)

-- enumFromTo
-- enumFromThenTo

toList :: Vec n a -> [a]
{-# INLINE toList #-}
toList (Vec vs) = G.toList vs

fromList :: [a] -> (forall n. Vec n a -> r) -> r
{-# INLINE fromList #-}
fromList xs f = G.fromList xs (f . Vec)

stream :: Vec n a -> Stream a
{-# INLINE stream #-}
stream (Vec vs) = G.stream vs

unstream :: Stream a -> (forall n. Vec n a -> r) -> r
{-# INLINE unstream #-}
unstream s f = G.unstream s (f . Vec)

streamR :: Vec n a -> Stream a
{-# INLINE streamR #-}
streamR (Vec vs) = G.streamR vs

unstreamR :: Stream a -> (forall n. Vec n a -> r) -> r
{-# INLINE unstreamR #-}
unstreamR s f = G.unstreamR s (f . Vec)

new :: New a -> (forall n. Vec n a -> r) -> r
{-# INLINE new #-}
new n f = G.new n (f . Vec)

allFin :: Nat n => n -> Vec n (Fin n)
{-# INLINE allFin #-}
allFin = Vec . G.allFin

