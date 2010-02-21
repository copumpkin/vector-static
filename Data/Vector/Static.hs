{-# LANGUAGE TypeOperators #-}
module Data.Vector.Static where

import qualified Data.Vector as V
import qualified Data.Vector.Generic.Static as G

import Data.Nat
import Data.Fin

import Control.Arrow

newtype Vec n a = Vec { unVec :: G.Vec n V.Vector a }

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

map :: (a -> b) -> Vec n a -> Vec n b
map f (Vec vs) = Vec (G.map f vs)

imap :: (Fin n -> a -> b) -> Vec n a -> Vec n b
imap f (Vec vs) = Vec (G.imap f vs)

zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith f (Vec as) (Vec bs) = Vec (G.zipWith f as bs)

izipWith :: (Fin n -> a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
izipWith f (Vec as) (Vec bs) = Vec (G.izipWith f as bs)

zip :: Vec n a -> Vec n b -> Vec n (a, b)
zip (Vec as) (Vec bs) = Vec (G.zip as bs)

unzip :: Vec n (a, b) -> (Vec n a, Vec n b)
unzip (Vec abs) = Vec *** Vec $ G.unzip abs

concatMap :: (a -> Vec n b) -> Vec m a -> Vec (m :*: n) b
concatMap f (Vec as) = Vec (G.concatMap (unVec . f) as)

init :: Vec (S n) a -> Vec n a
init (Vec vs) = Vec (G.init vs)

last :: Vec (S n) a -> a
last (Vec vs) = G.last vs

head :: Vec (S n) a -> a
head (Vec vs) = G.head vs

tail :: Vec (S n) a -> Vec n a
tail (Vec vs) = Vec (G.tail vs)

(++) :: Vec m a -> Vec n a -> Vec (m :+: n) a
Vec ms ++ Vec ns = Vec (ms G.++ ns)


allFin :: Nat n => Vec n (Fin n)
allFin = Vec G.allFin

generate :: Nat n => (Fin n -> a) -> Vec n a
generate f = Vec (G.generate f)

(!) :: Vec n a -> Fin n -> a
Vec vs ! i = vs G.! i

backpermute :: Vec m a -> Vec n (Fin m) -> Vec n a
backpermute (Vec vs) (Vec is) = Vec (G.backpermute vs is)

reverse :: Vec n a -> Vec n a
reverse (Vec vs) = Vec (G.reverse vs)

findIndex :: (a -> Bool) -> Vec n a -> Maybe (Fin n)
findIndex p (Vec vs) = G.findIndex p vs

findIndices :: (a -> Bool) -> Vec n a -> Vec m (Fin n) -- should we return proof that m <= n?
findIndices p (Vec vs) = Vec (G.findIndices p vs)

elemIndex :: Eq a => a -> Vec n a -> Maybe (Fin n)
elemIndex x (Vec vs) = G.elemIndex x vs

elemIndices :: Eq a => a -> Vec n a -> Vec m (Fin n)
elemIndices x (Vec vs) = Vec (G.elemIndices x vs)

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
