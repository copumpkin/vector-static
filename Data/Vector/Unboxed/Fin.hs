{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Data.Vector.Unboxed.Fin where
  
import Data.Fin
import Data.Vector.Unboxed (Unbox)
import Data.Vector.Generic (Vector)
import Data.Vector.Generic.Mutable (MVector)

deriving instance (Vector v Int) => Vector v (Fin n)
deriving instance (MVector v Int) => MVector v (Fin n)
deriving instance Unbox (Fin n)