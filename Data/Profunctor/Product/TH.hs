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
-- \$('makeAdaptorAndInstanceInferrable' \"pFoo\" ''Foo)
-- @
--
-- The adaptor for a type @Foo@ is by convention called @pFoo@, but in
-- practice you can call it anything.  If you don't care to specify
-- the name @pFoo@ yourself you can use
--
-- @
-- \$('makeAdaptorAndInstanceInferrable'' ''Foo)
-- @
--
-- and it will be named @pFoo@ automatically.
--
-- @pFoo@ will have the type
--
-- @
-- pFoo :: 'Data.Profunctor.Product.ProductProfunctor' p
--      => Foo (p a a') (p b b') (p c c')
--      -> p (Foo a b c) (Foo a' b' c')
-- @
--
-- and the instance generated will be
--
-- @
-- instance ('Data.Profunctor.Product.ProductProfunctor' p, Default p a a', Default p b b', Default p c c')
--       => Default p (Foo a b c) (Foo a' b' c')
-- @
--
-- If you are confused about the meaning of @pFoo@ it may help to
-- consider the corresponding function that works with @Applicative@s
-- (its implementation is given below).
--
-- @
-- pFooApplicative :: 'Control.Applicative.Applicative' f
--                 => Foo (f a) (f b) (f c)
--                 -> f (Foo a b c)
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
-- instance 'Data.Profunctor.Product.Newtype.Newtype' Foo where
--   'Data.Profunctor.Product.Newtype.constructor' = Foo
--   'Data.Profunctor.Product.Newtype.field'       = \\(Foo x) -> x
-- @
--
-- which allows you to use the polymorphic function 'Data.Profunctor.Product.Newtype.pNewtype'
-- instead of @pFoo@.
--
-- If you prefer not to use Template Haskell then the generated code
-- can be written by hand because it is quite simple.  It corresponds
-- very closely to what we would do in the more familiar
-- @Applicative@ case.  For an @Applicative@ we would write
--
-- @
-- pFooApplicative :: 'Control.Applicative.Applicative' f
--                 => Foo (f a) (f b) (f c) -> f (Foo a b c)
-- pFooApplicative f = Foo 'Control.Applicative.<$>' foo f
--                         'Control.Applicative.<*>' bar f
--                         'Control.Applicative.<*>' baz f
-- @
--
-- whereas for a @ProductProfunctor@ we write
--
-- @
-- import "Data.Profunctor" ('Data.Profunctor.lmap')
-- import "Data.Profunctor.Product" (('Data.Profunctor.Product.***$'), ('Data.Profunctor.Product.****'))
--
-- pFoo :: 'Data.Profunctor.Product.ProductProfunctor' p
--      => Foo (p a a') (p b b') (p c c') -> p (Foo a b c) (Foo a' b' c')
-- pFoo f = Foo 'Data.Profunctor.Product.***$' 'Data.Profunctor.lmap' foo (foo f)
--              'Data.Profunctor.Product.****' 'Data.Profunctor.lmap' bar (bar f)
--              'Data.Profunctor.Product.****' 'Data.Profunctor.lmap' baz (baz f)
-- @
--
-- The 'Default' instance is then very simple.
--
-- @
-- instance ('Data.Profunctor.Product.ProductProfunctor' p, 'Data.Profunctor.Product.Default.Default' p a a', 'Data.Profunctor.Product.Default.Default' p b b', 'Data.Profunctor.Product.Default.Default' p c c')
--       => 'Data.Profunctor.Product.Default.Default' p (Foo a b c) (Foo a' b' c') where
--     'Data.Profunctor.Product.Default.def' = pFoo (Foo 'Data.Profunctor.Product.Default.def' 'Data.Profunctor.Product.Default.def' 'Data.Profunctor.Product.Default.def')
-- @


module Data.Profunctor.Product.TH where

import           Data.Profunctor.Product.Internal.TH  (makeAdaptorAndInstanceI)
import qualified Language.Haskell.TH                   as TH

-- | For example
--
-- @
-- \$(makeAdaptorAndInstanceInferrable \"pFoo\" ''Foo)
-- @
--
-- generates the 'Default' instance and the adaptor @pFoo@.
makeAdaptorAndInstanceInferrable :: String -> TH.Name -> TH.Q [TH.Dec]
makeAdaptorAndInstanceInferrable adaptorNameS =
  makeAdaptorAndInstanceI True (Just adaptorNameS)

-- | For example
--
-- @
-- \$(makeAdaptorAndInstanceInferrable' ''Foo)
-- @
--
-- generates the 'Default' instance and the adaptor @pFoo@.  The name
-- of the adaptor is chosen by prefixing the type name \"Foo\" with
-- the string \"p\".
makeAdaptorAndInstanceInferrable' :: TH.Name -> TH.Q [TH.Dec]
makeAdaptorAndInstanceInferrable' =
  makeAdaptorAndInstanceI True Nothing

-- | Use 'makeAdaptorAndInstanceInferrable' instead, because it
-- generates instances with better inference properties.
makeAdaptorAndInstance :: String -> TH.Name -> TH.Q [TH.Dec]
makeAdaptorAndInstance adaptorNameS =
  makeAdaptorAndInstanceI False (Just adaptorNameS)
{-# DEPRECATED makeAdaptorAndInstance "Use makeAdaptorAndInstanceInferrable instead" #-}

-- | Use 'makeAdaptorAndInstanceInferrable' instead, because it
-- generates instances with better inference properties.
makeAdaptorAndInstance' :: TH.Name -> TH.Q [TH.Dec]
makeAdaptorAndInstance' =
  makeAdaptorAndInstanceI False Nothing
{-# DEPRECATED makeAdaptorAndInstance' "Use makeAdaptorAndInstanceInferrable' instead" #-}
