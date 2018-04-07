module Data.Profunctor.Product.Class where

import           Data.Profunctor (Profunctor)
import qualified Data.Profunctor as Profunctor
import qualified Control.Applicative
--- ^^ This is a redundant import but it's needeed for Haddock links.
-- AIUI Haddock can't link to something you haven't imported.
--
--     https://github.com/haskell/haddock/issues/796

-- | 'ProductProfunctor' is a generalization of
-- 'Control.Applicative.Applicative'.
--
-- It has the usual 'Control.Applicative.Applicative' "output"
-- (covariant) parameter on the right.  Additionally it has an "input"
-- (contravariant) type parameter on the left.
--
-- 'ProductProfunctor' corresponds closely to
-- 'Control.Applicative.Applicative' as laid out in the following
-- table.
--
-- @
-- | Correspondence between Applicative and ProductProfunctor
-- |
-- |  'Control.Applicative.Applicative' f           'ProductProfunctor' p
-- |
-- |  'Control.Applicative.pure'                    'purePP'
-- |    :: b -> f b             :: b -> p a b
-- |
-- |  ('Control.Applicative.<$>')                   ('Data.Profunctor.Product.***$')
-- |    :: (b -> b')            :: (b -> b')
-- |    -> f b                  -> p a b
-- |    -> f b'                 -> p a b'
-- |
-- |  ('Control.Applicative.<*>')                   ('****')
-- |    :: f (b -> b')          :: p a (b -> b')
-- |    -> f b                  -> p a b
-- |    -> f b'                 -> p a b'
-- @
--
-- It's easy to make instances of 'ProductProfunctor'.  Just make
-- instances
--
-- @
--  instance 'Profunctor' MyProductProfunctor where
--    ...
--
--  instance 'Control.Applicative.Applicative' (MyProductProfunctor a) where
--    ...
-- @
--
-- and then write
--
-- @
--  instance 'ProductProfunctor' MyProductProfunctor where
--    'purePP' = 'Control.Applicative.pure'
--    ('****') = ('Control.Applicative.<*>')
-- @
class Profunctor p => ProductProfunctor p where
  -- | 'purePP' is the generalisation of @Applicative@'s
  -- 'Control.Applicative.pure'.
  --
  -- Aside from defining 'ProductProfunctor' instances you will
  -- probably never need to use this; @pure@ should be sufficient (if
  -- your 'ProductProfunctor' instance also has an @Applicative@
  -- instance).
  purePP :: b -> p a b
  purePP b = Profunctor.dimap (const ()) (const b) empty

  -- | '****' is the generalisation of @Applicative@'s
  -- 'Control.Applicative.<*>'.
  --
  -- Aside from defining 'ProductProfunctor' instances you will you
  -- will probably never need to use this; @\<*\>@ should be
  -- sufficient (if your 'ProductProfunctor' instance has also been
  -- given an @Applicative@ instance).
  (****) :: p a (b -> c) -> p a b -> p a c
  (****) f x = Profunctor.dimap dup (uncurry ($)) (f ***! x)
    where dup y = (y, y)

  -- | You probably never want to use 'empty' and it may be deprecated
  -- in a future version.
  empty  :: p () ()
  empty = purePP ()

  -- | You probably never want to use '***!' and it may be
  -- deprecated in a future version.
  (***!) :: p a b -> p a' b' -> p (a, a') (b, b')
  f ***! g = (,) `Profunctor.rmap` Profunctor.lmap fst f
                  **** Profunctor.lmap snd g

class Profunctor p => SumProfunctor p where
  -- Morally we should have 'zero :: p Void Void' but I don't think
  -- that would actually be useful
  (+++!) :: p a b -> p a' b' -> p (Either a a') (Either b b')
