module Data.Fin where

newtype Fin n = Fin Int
  deriving (Show, Eq, Ord)

zero :: Fin (S n)
zero = Fin 0

succ :: Fin n -> Fin (S n)
succ (Fin n) = Fin (n + 1)

addFin :: Fin x -> Fin y -> Fin (x :+: y)
addFin (Fin x) (Fin y) = Fin (x + y)
