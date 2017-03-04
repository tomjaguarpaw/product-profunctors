{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, LambdaCase,
             MultiParamTypeClasses, TypeOperators #-}
module Data.Profunctor.Product.Default.Class where

import GHC.Generics

import Data.Profunctor (Profunctor, dimap)
import Data.Profunctor.Product.Class

class Default p a b where
  -- Would rather call it "default", but that's a keyword
  def :: p a b
  default def :: (Profunctor p, Generic a, Generic b, GDefault p (Rep a) (Rep b)) => p a b
  def = gdef

-- * Generic instance for Default

class GDefault p f g where
  gdef1 :: p (f a) (g a)

instance ProductProfunctor p => GDefault p U1 U1 where
  gdef1 = dimap (const ()) (const U1) empty

instance (Profunctor p, GDefault p f g) => GDefault p (M1 i c f) (M1 i c g) where
  gdef1 = dimap unM1 M1 gdef1

instance (Profunctor p, Default p c c') => GDefault p (K1 i c) (K1 i c') where
  gdef1 = dimap unK1 K1 def

instance (ProductProfunctor p, GDefault p f f', GDefault p g g') => GDefault p (f :*: g) (f' :*: g') where
  gdef1 = dimap (\(x :*: y) -> (x, y)) (uncurry (:*:)) $ gdef1 ***! gdef1

instance (SumProfunctor p, GDefault p f f', GDefault p g g') => GDefault p (f :+: g) (f' :+: g') where
  gdef1 = dimap sumToEither eitherToSum $ gdef1 +++! gdef1
    where
      eitherToSum = \case
        Left  x -> L1 x
        Right x -> R1 x
      sumToEither = \case
        L1 x -> Left  x
        R1 x -> Right x

gdef :: (Profunctor p, Generic a, Generic b, GDefault p (Rep a) (Rep b)) => p a b
gdef = dimap from to gdef1
