{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             FlexibleContexts, PolyKinds, TemplateHaskell #-}

module Data.Profunctor.Product.Default
  ( module Data.Profunctor.Product.Default
  , module Data.Profunctor.Product.Default.Class
  ) where

import Control.Applicative (Const (Const))
import Data.Functor.Identity (Identity (Identity))
import Data.Profunctor (Profunctor, dimap)
-- TODO: vv this imports a lot of names.  Should we list them all?
import Data.Profunctor.Product
import Data.Tagged (Tagged (Tagged))

import Data.Profunctor.Product.Default.Class
import Data.Profunctor.Product.Tuples.TH (mkDefaultNs, maxTupleSize)

-- | This will be deprecated in a future version
cdef :: Default (PPOfContravariant u) a a => u a
cdef = unPPOfContravariant def

instance (Profunctor p, Default p a b) => Default p (Identity a) (Identity b)
  where
    def = dimap (\(Identity a) -> a) Identity def

instance (Profunctor p, Default p a b) => Default p (Const a c) (Const b c')
  where
    def = dimap (\(Const a) -> a) Const def

instance (Profunctor p, Default p a b) => Default p (Tagged s a) (Tagged s' b)
  where
    def = dimap (\(Tagged a) -> a) Tagged def

instance (Profunctor p, Default p a (Maybe b)) =>
    Default p (Const a c) (Maybe (Const b c'))
  where
    def = dimap (\(Const a) -> a) (fmap Const) def

instance (Profunctor p, Default p (Maybe a) b) =>
    Default p (Maybe (Const a c)) (Const b c')
  where
    def = dimap (fmap (\(Const a) -> a)) Const def

instance (Profunctor p, Default p (Maybe a) (Maybe b)) =>
    Default p (Maybe (Const a c)) (Maybe (Const b c'))
  where
    def = dimap (fmap (\(Const a) -> a)) (fmap Const) def

instance (Profunctor p, Default p a (Maybe b)) =>
    Default p (Identity a) (Maybe (Identity b))
  where
    def = dimap (\(Identity a) -> a) (fmap Identity) def

instance (Profunctor p, Default p (Maybe a) b) =>
    Default p (Maybe (Identity a)) (Identity b)
  where
    def = dimap (fmap (\(Identity a) -> a)) Identity def

instance (Profunctor p, Default p (Maybe a) (Maybe b)) =>
    Default p (Maybe (Identity a)) (Maybe (Identity b))
  where
    def = dimap (fmap (\(Identity a) -> a)) (fmap Identity) def

instance (Profunctor p, Default p a (Maybe b)) =>
    Default p (Tagged s a) (Maybe (Tagged s' b))
  where
    def = dimap (\(Tagged a) -> a) (fmap Tagged) def

instance (Profunctor p, Default p (Maybe a) b) =>
    Default p (Maybe (Tagged s a)) (Tagged s' b)
  where
    def = dimap (fmap (\(Tagged a) -> a)) Tagged def

instance (Profunctor p, Default p (Maybe a) (Maybe b)) =>
    Default p (Maybe (Tagged s a)) (Maybe (Tagged s' b))
  where
    def = dimap (fmap (\(Tagged a) -> a)) (fmap Tagged) def

instance (Profunctor p, Default p a [b]) => Default p (Const a c) [Const b c']
  where
    def = dimap (\(Const a) -> a) (fmap Const) def

instance (Profunctor p, Default p [a] b) => Default p [Const a c] (Const b c')
  where
    def = dimap (fmap (\(Const a) -> a)) Const def

instance (Profunctor p, Default p [a] [b]) =>
    Default p [Const a c] [Const b c']
  where
    def = dimap (fmap (\(Const a) -> a)) (fmap Const) def

instance (Profunctor p, Default p a [b]) =>
    Default p (Identity a) [Identity b]
  where
    def = dimap (\(Identity a) -> a) (fmap Identity) def

instance (Profunctor p, Default p [a] b) =>
    Default p [Identity a] (Identity b)
  where
    def = dimap (fmap (\(Identity a) -> a)) Identity def

instance (Profunctor p, Default p [a] [b]) =>
    Default p [Identity a] [Identity b]
  where
    def = dimap (fmap (\(Identity a) -> a)) (fmap Identity) def

instance (Profunctor p, Default p a [b]) =>
    Default p (Tagged s a) [Tagged s' b]
  where
    def = dimap (\(Tagged a) -> a) (fmap Tagged) def

instance (Profunctor p, Default p [a] b) =>
    Default p [Tagged s a] (Tagged s' b)
  where
    def = dimap (fmap (\(Tagged a) -> a)) Tagged def

instance (Profunctor p, Default p [a] [b]) =>
    Default p [Tagged s a] [Tagged s' b]
  where
    def = dimap (fmap (\(Tagged a) -> a)) (fmap Tagged) def


mkDefaultNs (0:[2..maxTupleSize])
