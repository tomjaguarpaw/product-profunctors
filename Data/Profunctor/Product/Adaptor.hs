-- |
--
-- Adaptors generalize traversals in two ways:
--
-- - they may focus on values of different types;
--
-- - the type of transformation is an abstract product profunctor @p a b@,
--   rather than a function type @a -> f b@.
--
-- > (a -> f b)         -> (a, a) -> f (b, b)   -- Traversal
-- > (p a1 b1, p a2 b2) -> p (a1, a2) (b1, b2)  -- Adaptor
--
-- This module provides a generic implementation of adaptors
-- and a type synonym for convenience.
--
-- === Example
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
-- import "GHC.Generics"
--
-- data Foo a b c = Foo { fooA :: a, fooB :: b, fooC :: c } deriving 'GHC.Generics.Generic'
--
-- pFoo :: 'Data.Profunctor.Product.ProductProfunctor' p => 'Adaptor' p (Foo (p a a') (p b b') (p c c'))
-- pFoo = 'genericAdaptor'
-- @
--
-- is equivalent to
--
-- @
-- pFoo :: 'Data.Profunctor.Product.ProductProfunctor' p =>
--         Foo (p a a') (p b b') (p c c') -> p (Foo a b c) (Foo a' b' c')
-- pFoo (Foo a b c) = Foo
--   'Data.Profunctor.Product.***$' 'Data.Profunctor.lmap' fooA a
--   'Data.Profunctor.Product.****' 'Data.Profunctor.lmap' fooB b
--   'Data.Profunctor.Product.****' 'Data.Profunctor.lmap' fooC c
-- @
--
-- To use the type synonym 'Adaptor' in versions of GHC older than 8.0.1,
-- @Foo@ must be an instance of 'Unzippable'. You may simply declare a
-- default instance:
--
-- @
-- instance 'Unzippable' Foo
-- @

module Data.Profunctor.Product.Adaptor
  ( genericAdaptor
  , Adaptor
  , Unzippable
  ) where

import Data.Profunctor.Product.Internal.Adaptor
