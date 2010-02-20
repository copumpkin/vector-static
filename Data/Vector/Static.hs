{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, TypeFamilies, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances #-}
import Prelude hiding (succ, replicate, map)
import qualified Data.Vector as V
import Unsafe.Coerce
import Control.Arrow

newtype I x = I { unI :: x }
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


newtype Vec n a = Vec { unVec :: V.Vector a }
  deriving (Show, Eq)

empty :: Vec Z a
empty = Vec V.empty

singleton :: a -> Vec (S Z) a
singleton = Vec . V.singleton

cons :: a -> Vec n a -> Vec (S n) a
cons x (Vec xs) = Vec (V.cons x xs)

snoc :: Vec n a -> a -> Vec (S n) a
snoc (Vec xs) x = Vec (V.snoc xs x)

replicate :: forall a n. Nat n => a -> Vec n a
replicate = Vec . V.replicate (natToInt (witnessNat :: n))

map :: (a -> b) -> Vec n a -> Vec n b
map f (Vec vs) = Vec (V.map f vs)

imap :: (Fin n -> a -> b) -> Vec n a -> Vec n b
imap f (Vec vs) = Vec (V.imap (f . Fin) vs)

zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith f (Vec as) (Vec bs) = Vec (V.zipWith f as bs) -- is there an unsafe zipWith?

izipWith :: (Fin n -> a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
izipWith f (Vec as) (Vec bs) = Vec (V.izipWith (f . Fin) as bs) -- is there an unsafe zipWith?

zip :: Vec n a -> Vec n b -> Vec n (a, b)
zip (Vec as) (Vec bs) = Vec (V.zip as bs) -- unsafe zip?

unzip :: Vec n (a, b) -> (Vec n a, Vec n b)
unzip (Vec abs) = Vec *** Vec $ V.unzip abs

concatMap :: (a -> Vec n b) -> Vec m a -> Vec (m :*: n) b
concatMap f (Vec as) = Vec (V.concatMap (unVec . f) as)

init :: Vec (S n) a -> Vec n a
init (Vec vs) = Vec (V.unsafeInit vs)

last :: Vec (S n) a -> a
last (Vec vs) = V.unsafeLast vs

head :: Vec (S n) a -> a
head (Vec vs) = V.unsafeHead vs

tail :: Vec (S n) a -> Vec n a
tail (Vec vs) = Vec (V.unsafeTail vs)

(++) :: Vec m a -> Vec n a -> Vec (m :+: n) a
Vec ms ++ Vec ns = Vec (ms V.++ ns)


newtype Fin n = Fin Int
  deriving (Show, Eq, Ord)

zero :: Fin (S n)
zero = Fin 0

succ :: Fin n -> Fin (S n)
succ (Fin n) = Fin (n + 1)

addFin :: Fin x -> Fin y -> Fin (x :+: y)
addFin (Fin x) (Fin y) = Fin (x + y)

allFin :: forall n. Nat n => Vec n (Fin n)
allFin = Vec (V.generate (natToInt (witnessNat :: n)) Fin)

generate :: forall n a. Nat n => (Fin n -> a) -> Vec n a
generate f = Vec (V.generate (natToInt (witnessNat :: n)) (f . Fin))

(!) :: Vec n a -> Fin n -> a
Vec vs ! Fin i = V.unsafeIndex vs i

backpermute :: Vec m a -> Vec n (Fin m) -> Vec n a
backpermute (Vec vs) (Vec is) = Vec (V.unsafeBackpermute vs (unsafeCoerce is))

reverse :: Vec n a -> Vec n a
reverse (Vec vs) = Vec (V.reverse vs)

findIndex :: (a -> Bool) -> Vec n a -> Maybe (Fin n)
findIndex p (Vec vs) = fmap Fin $ V.findIndex p vs

findIndices :: (a -> Bool) -> Vec n a -> Vec m (Fin n) -- should we return proof that m <= n?
findIndices p (Vec vs) = Vec (V.map Fin $ V.findIndices p vs)

elemIndex :: Eq a => a -> Vec n a -> Maybe (Fin n)
elemIndex x (Vec vs) = fmap Fin $ V.elemIndex x vs

elemIndices :: Eq a => a -> Vec n a -> Vec m (Fin n)
elemIndices x (Vec vs) = Vec (V.map Fin $ V.elemIndices x vs)

minimum :: Ord a => Vec (S n) a -> a
minimum (Vec vs) = V.minimum vs

minimumBy :: (a -> a -> Ordering) -> Vec (S n) a -> a
minimumBy c (Vec vs) = V.minimumBy c vs

minIndex :: Ord a => Vec (S n) a -> Fin (S n)
minIndex (Vec vs) = Fin (V.minIndex vs)

minIndexBy :: (a -> a -> Ordering) -> Vec (S n) a -> Fin (S n)
minIndexBy c (Vec vs) = Fin (V.minIndexBy c vs)

maximum :: Ord a => Vec (S n) a -> a
maximum (Vec vs) = V.maximum vs

maximumBy :: (a -> a -> Ordering) -> Vec (S n) a -> a
maximumBy c (Vec vs) = V.maximumBy c vs

maxIndex :: Ord a => Vec (S n) a -> Fin (S n)
maxIndex (Vec vs) = Fin (V.maxIndex vs)

maxIndexBy :: (a -> a -> Ordering) -> Vec (S n) a -> Fin (S n)
maxIndexBy c (Vec vs) = Fin (V.maxIndexBy c vs)
