module Data.Profunctor.Product.Newtype where

import qualified Data.Profunctor as P

class Newtype t where
  constructor :: a -> t a
  field       :: t a -> a

ppNewtype :: (P.Profunctor p, Newtype t) => p a b -> p (t a) (t b)
ppNewtype = P.dimap field constructor
