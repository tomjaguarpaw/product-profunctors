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
-- \"adaptor\" with the following splice:
--
-- @
--  $(makeAdaptorAndInstance \"pFoo\" ''Foo)
-- @
--
-- The adaptor for a type @Foo@ is by convention called @pFoo@, but in
-- practice you can call it anything.  If you don't care to specify
-- the name @pFoo@ yourself you can use
--
-- @
--  $(makeAdaptorAndInstance' ''Foo)
-- @
--
-- and it will be named @pFoo@ automatically.
--
-- @pFoo@ will have the type
--
-- @
-- pFoo :: ProductProfunctor p =>
--         Foo (p a a') (p b b') (p c c') -> p (Foo a b c) (Foo a' b' c')
-- @
--
-- and the instance generated will be
--
-- @
-- instance (ProductProfunctor p, Default p a a', Default p b b', Default p c c')
--       => Default p (Foo a b c) (Foo a' b' c')
-- @
--
-- If you are confused about the meaning of @pFoo@ it may help to
-- consider the corresponding function that works with @Applicative@s
-- (its implementation is given below).
--
-- @
-- pFooApplicative :: Applicative f =>
--         Foo (f a) (f b) (f c) -> f (Foo a b c)
-- @
--
-- The product-profunctor \"adaptor\" (in this case @pFoo@) is a
-- generalization of @Data.Traversable.sequence@ in two different
-- ways.  Firstly it works on datatypes with multiple type parameters.
-- Secondly it works on 'ProductProfunctor's, which are themselves a
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
--
-- If you prefer not to use Template Haskell then the generated code
-- can be written by hand because it is quite simple.  It corresponds
-- very closely to what you would do in the more familiar
-- @Applicative@ case.  For an @Applicative@ we would write
--
-- @
-- pFooApplicative :: Applicative f
--                 => Foo (f a) (f b) (f c) -> f (Foo a b c)
-- pFooApplicative f = Foo \<$\> foo f
--                         \<*\> bar f
--                         \<*\> baz f
-- @
--
-- whereas for a @ProductProfunctor@ we write
--
-- @
-- import Data.Profunctor (lmap)
-- import Data.Profunctor.Product ((***$), (****))
--
-- pFoo :: ProductProfunctor p
--      => Foo (p a a') (p b b') (p c c') -> p (Foo a b c) (Foo a' b' c')
-- pFoo f = Foo ***$ lmap foo (foo f)
--              **** lmap bar (bar f)
--              **** lmap baz (baz f)
-- @
--
-- The 'Default' instance is then very simple.
--
-- @
-- instance (ProductProfunctor p, Default p a a', Default p b b', Default p c c')
--       => Default p (Foo a b c) (Foo a' b' c') where
--     def = pFoo (Foo def def def)
-- @


module Data.Profunctor.Product.TH where

import           Data.Profunctor.Product.Internal.TH  (makeAdaptorAndInstanceI)
import qualified Language.Haskell.TH                   as TH

-- | For example
--
-- @
-- $(makeAdaptorAndInstance \"pFoo\" ''Foo)
-- @
--
-- generates the 'Default' instance and the adaptor @pFoo@.
makeAdaptorAndInstance :: String -> TH.Name -> TH.Q [TH.Dec]
makeAdaptorAndInstance adaptorNameS = makeAdaptorAndInstanceI (Just adaptorNameS)

-- | For example
--
-- @
-- $(makeAdaptorAndInstance ''Foo)
-- @
--
-- generates the 'Default' instance and the adaptor @pFoo@.  The name
-- of the adaptor is chosen by prefixing the type name \"Foo\" with
-- the string \"p\".
makeAdaptorAndInstance' :: TH.Name -> TH.Q [TH.Dec]
makeAdaptorAndInstance' = makeAdaptorAndInstanceI Nothing

