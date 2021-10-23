{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             FlexibleContexts, PolyKinds, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | For some 'Data.Profunctor.Product.ProductProfunctor's @p@ and
-- types @a@, @a'@ there is a unique most sensible value of @p a a'@.
-- 'Default' exists to automatically generate that unique most
-- sensible value for a product given unique most sensible values for
-- the base types. If the unique most sensible values of type @p a
-- a'@, @p b b'@ and @p c c'@ are
--
-- @
-- sensible_a :: p a a'
-- sensible_b :: p b b'
-- sensible_c :: p c c'
-- @
--
-- then the unique most sensible value of type @p (a, b, c) (a', b',
-- c')@ is
--
-- @
--'Data.Profunctor.Product.p3' (sensible_a, sebsible_b, sensible_c)
--     :: p (a, b, c) (a', b', c')
-- @
--
-- Therefore there is an instance
--
-- @
-- instance
--   ( 'Default' p a a'
--   , 'Default' p b b'
--   , 'Default' p c c'
--   )
-- => 'Default' p (a, b, c) (a', b', c')
--     where 'def' = 'Data.Profunctor.Product.p3' ('def', 'def', 'def')
-- @
--
-- which can be read as "if the unique most sensible values of types
-- ... are ... then the unique most sensible value of the 3-tuple is
-- given by composing them with 'p3'".  Naturally each different
-- product type has a different composition function.

module Data.Profunctor.Product.Default
  ( module Data.Profunctor.Product.Default.Class
  ) where

import Control.Applicative (Const (Const))
import Data.Functor.Identity (Identity (Identity))
import Data.Profunctor (Profunctor, dimap)
-- TODO: vv this imports a lot of names.  Should we list them all?
import Data.Profunctor.Product
import Data.Tagged (Tagged (Tagged))

import Data.Profunctor.Product.Default.Class
import Data.Profunctor.Product.Tuples.TH (mkDefaultNs, maxTupleSize)

instance (Profunctor p, Default p a b) => Default p (Identity a) (Identity b)
  where
    def = dimap (\(Identity a) -> a) Identity def

instance (Profunctor p, Default p a b) => Default p (Const a c) (Const b c')
  where
    def = dimap (\(Const a) -> a) Const def

instance (Profunctor p, Default p a b) => Default p (Tagged s a) (Tagged s' b)
  where
    def = dimap (\(Tagged a) -> a) Tagged def

mkDefaultNs (0:[2..maxTupleSize])
