module Data.Profunctor.Product.Class where

import           Data.Profunctor (Profunctor)
import qualified Data.Profunctor as Profunctor

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
-- If @p@ is an instance of 'ProductProfunctor' then @p a a'@
-- represents a sort of process for turning @a@s into @a'@s that can
-- be "laid out side-by-side" with other values of @p@ to form "wider"
-- processes.  For example, if I have
--
-- @
-- a :: p a x -- a process for turning as into xs
-- b :: p b y -- a process for turning bs into ys
-- c :: p c z -- a process for turning cs into zs
-- @
--
-- then I can combine them using 'p3' to get
--
-- @
-- p3 a b c :: p (a, b, c) (x, y, z)
-- -- a process for turning (a, b, c)s into (x, y, z)s
-- @
--
-- You would typically compose 'ProductProfunctor's using
-- 'Profunctors''s 'Profunctor.lmap' and 'Applicative''s 'pure',
-- '<$>' / 'fmap' and '<*>'.
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
  -- Morally we should have 'zero :: p Void Void' but I don't think
  -- that would actually be useful
  (+++!) :: p a b -> p a' b' -> p (Either a a') (Either b b')
