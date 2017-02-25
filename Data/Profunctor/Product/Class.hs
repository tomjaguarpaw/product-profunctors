module Data.Profunctor.Product.Class where

import           Data.Profunctor (Profunctor)
import qualified Data.Profunctor as Profunctor

-- | 'ProductProfunctor' is a generalization of 'Applicative'.
--
-- It has the usual 'Applicative' "output" (covariant) parameter on
-- the right.  Additionally it has an "input" (contravariant) type
-- parameter on the left.
--
-- You will find it easier to see the correspondence between
-- 'ProductProfunctor' and 'Applicative' if you look at @purePP@,
-- @(***$)@, and @(****)@, which correspond to @pure@, @(\<$\>)@, and
-- @(\<*\>)@ respectively.
--
-- @
-- | Correspondence between Applicative and ProductProfunctor
-- |
-- |  Applicative f           ProductProfunctor p
-- |
-- |  pure                    purePP
-- |    :: b -> f b             :: b -> p a b
-- |
-- |  (\<$\>)                   (***$)
-- |    :: (b -> b')            :: (b -> b')
-- |    -> f b                  -> p a b
-- |    -> f b'                 -> p a b'
-- |
-- |  (\<*\>)                   (****)
-- |    :: f (b -> b')          :: p a (b -> b')
-- |    -> f b                  -> p a b
-- |    -> f b'                 -> p a b'
-- @
--
-- It's easy to make instances of 'ProductProfunctor'.  Just make
-- instances
--
-- @
--  instance Profunctor MyProductProfunctor where
--    ...
--
--  instance Applicative (MyProductProfunctor a) where
--    ...
-- @
--
-- and then write
--
-- @
--  instance ProductProfunctor MyProductProfunctor where
--    purePP = pure
--    (****) = (\<*\>)
-- @
class Profunctor p => ProductProfunctor p where
  -- | 'purePP' is the generalisation of @Applicative@'s @pure@.
  --
  -- Aside from defining 'ProductProfunctor' instances you will
  -- probably never need to use this; @pure@ should be sufficient (if
  -- your 'ProductProfunctor' instance also has an @Applicative@
  -- instance).
  purePP :: ProductProfunctor p => b -> p a b
  purePP b = Profunctor.dimap (const ()) (const b) empty

  -- | '****' is the generalisation of @Applicative@'s @\<*\>@.
  --
  -- Aside from defining 'ProductProfunctor' instances you will you
  -- will probably never need to use this; @\<*\>@ should be
  -- sufficient (if your 'ProductProfunctor' instance has also been
  -- given an @Applicative@ instance).
  (****) :: ProductProfunctor p => p a (b -> c) -> p a b -> p a c
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
