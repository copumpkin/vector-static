{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
module Data.Fin where

import Data.Nat

newtype Fin n = Fin Int
  deriving (Show, Eq, Ord)

zero :: Fin (S n)
zero = Fin 0

succ :: Fin n -> Fin (S n)
succ (Fin n) = Fin (n + 1)

pred :: Fin n -> Fin n
pred (Fin 0) = Fin 0
pred (Fin n) = Fin (n - 1)

addFin :: Fin x -> Fin y -> Fin (x :+: y)
addFin (Fin x) (Fin y) = Fin $ x + y

mulFin :: Fin x -> Fin y -> Fin (x :*: y)
mulFin (Fin x) (Fin y) = Fin $ x * y

-- 5 * Fin 13 is max 5 * 12 = 60, or Fin 61
mulNatFin :: Nat x => x -> Fin (S y) -> Fin (S (x :*: y))
mulNatFin x (Fin y) = Fin $ natToInt x * y

raise :: k -> Fin n -> Fin (n :+: k)
raise _ (Fin i) = Fin i

intToFin :: forall n. Nat n => Int -> Maybe (Fin n)
intToFin i | i >= natToInt (witnessNat :: n) || i < 0 = Nothing
           | otherwise = Just (Fin i)

finToInt :: Fin n -> Int
finToInt (Fin i) = i

natToFin :: Nat n => n -> Fin (S n)
natToFin = Fin . natToInt