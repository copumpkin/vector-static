{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, TypeFamilies, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances #-}
import Prelude hiding (succ, replicate, map)
import qualified Data.Vector.Generic as G
import Unsafe.Coerce
import Control.Arrow

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
type instance (:*:) (S m) n = n :+: (m :*: n)


newtype Vec n v a = Vec { unVec :: v a }
  deriving (Show, Eq)

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

map :: (G.Vector v a, G.Vector v b) => (a -> b) -> Vec n v a -> Vec n v b
map f (Vec vs) = Vec (G.map f vs)

imap :: (G.Vector v a, G.Vector v b) => (Fin n -> a -> b) -> Vec n v a -> Vec n v b
imap f (Vec vs) = Vec (G.imap (f . Fin) vs)

zipWith :: (G.Vector v a, G.Vector v b, G.Vector v c) => (a -> b -> c) -> Vec n v a -> Vec n v b -> Vec n v c
zipWith f (Vec as) (Vec bs) = Vec (G.zipWith f as bs) -- is there an unsafe zipWith?

izipWith :: (G.Vector v a, G.Vector v b, G.Vector v c) => (Fin n -> a -> b -> c) -> Vec n v a -> Vec n v b -> Vec n v c
izipWith f (Vec as) (Vec bs) = Vec (G.izipWith (f . Fin) as bs) -- is there an unsafe zipWith?

zip :: (G.Vector v a, G.Vector v b, G.Vector v (a, b)) => Vec n v a -> Vec n v b -> Vec n v (a, b)
zip (Vec as) (Vec bs) = Vec (G.zip as bs) -- unsafe zip?

unzip :: (G.Vector v a, G.Vector v b, G.Vector v (a, b)) => Vec n v (a, b) -> (Vec n v a, Vec n v b)
unzip (Vec abs) = Vec *** Vec $ G.unzip abs

concatMap :: (G.Vector v a, G.Vector v b) => (a -> Vec n v b) -> Vec m v a -> Vec (m :*: n) v b
concatMap f (Vec as) = Vec (G.concatMap (unVec . f) as)

init :: G.Vector v a => Vec (S n) v a -> Vec n v a
init (Vec vs) = Vec (G.unsafeInit vs)

last :: G.Vector v a => Vec (S n) v a -> a
last (Vec vs) = G.unsafeLast vs

head :: G.Vector v a => Vec (S n) v a -> a
head (Vec vs) = G.unsafeHead vs

tail :: G.Vector v a => Vec (S n) v a -> Vec n v a
tail (Vec vs) = Vec (G.unsafeTail vs)

(++) :: G.Vector v a => Vec m v a -> Vec n v a -> Vec (m :+: n) v a
Vec ms ++ Vec ns = Vec (ms G.++ ns)


newtype Fin n = Fin Int
  deriving (Show, Eq, Ord)

zero :: Fin (S n)
zero = Fin 0

succ :: Fin n -> Fin (S n)
succ (Fin n) = Fin (n + 1)

addFin :: Fin x -> Fin y -> Fin (x :+: y)
addFin (Fin x) (Fin y) = Fin (x + y)

allFin :: forall n v. (Nat n, G.Vector v (Fin n)) => Vec n v (Fin n)
allFin = Vec (G.generate (natToInt (witnessNat :: n)) Fin)

generate :: forall n v a. (Nat n, G.Vector v a) =>  (Fin n -> a) -> Vec n v a
generate f = Vec (G.generate (natToInt (witnessNat :: n)) (f . Fin))

(!) :: G.Vector v a => Vec n v a -> Fin n -> a
Vec vs ! Fin i = G.unsafeIndex vs i

backpermute :: (G.Vector v a, G.Vector v Int) => Vec m v a -> Vec n v (Fin m) -> Vec n v a
backpermute (Vec vs) (Vec is) = Vec (G.unsafeBackpermute vs (unsafeCoerce is))

reverse :: G.Vector v a => Vec n v a -> Vec n v a
reverse (Vec vs) = Vec (G.reverse vs)

findIndex :: G.Vector v a => (a -> Bool) -> Vec n v a -> Maybe (Fin n)
findIndex p (Vec vs) = fmap Fin $ G.findIndex p vs

findIndices :: (G.Vector v a, G.Vector v Int, G.Vector v (Fin n)) => (a -> Bool) -> Vec n v a -> Vec m v (Fin n) -- should we return proof that m <= n?
findIndices p (Vec vs) = Vec (G.map Fin $ G.findIndices p vs)

elemIndex :: G.Vector v a => Eq a => a -> Vec n v a -> Maybe (Fin n)
elemIndex x (Vec vs) = fmap Fin $ G.elemIndex x vs

elemIndices :: (G.Vector v a, G.Vector v Int, G.Vector v (Fin n)) => Eq a => a -> Vec n v a -> Vec m v (Fin n)
elemIndices x (Vec vs) = Vec (G.map Fin $ G.elemIndices x vs)

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
