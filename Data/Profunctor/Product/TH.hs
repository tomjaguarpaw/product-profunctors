{-# LANGUAGE TemplateHaskell #-}

-- | If you have a data declaration which is a polymorphic product,
-- for example
--
-- @
-- data Foo a b c = Foo a b c
-- @
--
-- or
--
-- @
-- data Foo a b c = Foo { foo :: a, bar :: b, baz :: c }
-- @
--
-- then you can use Template Haskell to automatically derive the
-- product-profunctor 'Default' instances and product-profunctor
-- \"adaptor\" with the following import and splice:
--
-- @
-- $(makeAdaptorAndInstance \"pFoo\" ''Foo)
-- @
--
-- * The adaptor for a type Foo is by convention called pFoo, but in
-- practice you can call it anything.
--
-- The instance generated will be
--
-- @
-- instance (ProductProfunctor p, Default p a a', Default p b b', Default p c c')
--       => Default p (Foo a b c) (Foo a' b' c')
-- @
--
-- and pFoo will have the type
--
-- @
-- pFoo :: ProductProfunctor p =>
--         Foo (p a a') (p b b') (p c c') -> p (Foo a b c) (Foo a' b' c')
-- @
--
-- If you don't care to specify the name @pFoo@ yourself you can use
--
-- @
-- $(makeAdaptorAndInstance' ''Foo)
-- @
--
-- and will be nameed @pFoo@ automatically.
--
-- The product-profunctor "adaptor" is a generalization of
-- @Data.Traversable.Sequence@ in two different ways.  Firstly it
-- works on datatypes with multiple type parameters.  Secondly it
-- works on 'ProductProfunctor's, which are themselves a
-- generalization of 'Applicative's.
--
-- If your type has only one field, for example
--
-- @
-- data Foo a = Foo a
-- @
--
-- or
--
-- @
-- newtype Foo a = Foo a
-- @
--
-- then you will also get the instance
--
-- @
-- instance 'N.Newtype' Foo where
--   'N.constructor' = Foo
--   'N.field'       = \(Foo x) -> x
-- @
--
-- which allows you to use the polymorphic function 'N.pNewtype'
-- instead of @pFoo@.

module Data.Profunctor.Product.TH where

import           Data.Profunctor.Product.Internal.TH  (makeAdaptorAndInstanceI)
import qualified Language.Haskell.TH                   as TH

makeAdaptorAndInstance' :: TH.Name -> TH.Q [TH.Dec]
makeAdaptorAndInstance' = makeAdaptorAndInstanceI Nothing

makeAdaptorAndInstance :: String -> TH.Name -> TH.Q [TH.Dec]
makeAdaptorAndInstance adaptorNameS = makeAdaptorAndInstanceI (Just adaptorNameS)
