module Data.Profunctor.Product.Class where

import           Data.Profunctor (Profunctor)
import qualified Data.Profunctor as Profunctor
import           Data.Void (Void)

--- vv These are redundant imports but they're needeed for Haddock
--- links. AIUI Haddock can't link to something you haven't imported.
--
--     https://github.com/haskell/haddock/issues/796
import qualified Control.Applicative
import qualified Data.Profunctor

-- | 'ProductProfunctor' is a generalization of
-- 'Control.Applicative.Applicative'.
-- It has the usual 'Control.Applicative.Applicative' "output"
-- (covariant) parameter on the right.  Additionally it has an "input"
-- (contravariant) type parameter on the left.
--
-- The methods for 'ProductProfunctor' correspond closely to those for
-- 'Control.Applicative.Applicative' as laid out in the following
-- table.
-- The only difference between them is that the 'ProductProfunctor'
-- has a contravariant type parameter on the left.  We can use the
-- contravariant to compose them in nice ways as described at
-- "Data.Profunctor.Product".
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
  -- (You probably won't need to use this except to define
  -- 'ProductProfunctor' instances.  In your own code @pure@ should be
  -- sufficient.)
  purePP :: b -> p a b
  purePP b = Profunctor.dimap (const ()) (const b) empty

  -- | '****' is the generalisation of @Applicative@'s
  -- 'Control.Applicative.<*>'.
  --
  -- (You probably won't need to use this except to define
  -- 'ProductProfunctor' instances.  In your own code @\<*\>@ should
  -- be sufficient.)
  (****) :: p a (b -> c) -> p a b -> p a c
  (****) f x = Profunctor.dimap dup (uncurry ($)) (f ***! x)
    where dup y = (y, y)

  -- | Use @pure ()@ instead.  @empty@ may be deprecated in a future
  -- version.
  empty  :: p () ()
  empty = purePP ()

  -- | Use @\\f g -> (,) 'Control.Applicative.<$>'
  -- 'Data.Profunctor.lmap' fst f 'Control.Applicative.<*>'
  -- 'Data.Profunctor.lmap' snd g@ instead.
  -- @(***!)@ may be deprecated in a future version.
  (***!) :: p a b -> p a' b' -> p (a, a') (b, b')
  f ***! g = (,) `Profunctor.rmap` Profunctor.lmap fst f
                  **** Profunctor.lmap snd g

class Profunctor p => SumProfunctor p where
  (+++!) :: p a b -> p a' b' -> p (Either a a') (Either b b')

  -- | Unit of @('+++!')@. You might expect this to be @p Void Void@,
  -- but that can be immediately generalised by
  -- @'Data.Profunctor.rmap' 'Data.Void.absurd'@.
  void :: p Void a
