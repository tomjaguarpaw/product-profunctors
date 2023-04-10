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
-- p :: p a x -- a process for turning as into xs
-- q :: p b y -- a process for turning bs into ys
-- r :: p c z -- a process for turning cs into zs
-- @
--
-- then I can combine them using 'p3' to get
--
-- @
-- p3 p q r :: p (a, b, c) (x, y, z)
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
class Profunctor p => SemiproductProfunctor p where
  -- | '****' is the generalisation of @Applicative@'s
  -- 'Control.Applicative.<*>'.
  --
  -- (You probably won't need to use this except to define
  -- 'SemiproductProfunctor' instances.  In your own code @\<*\>@
  -- should be sufficient.)
  (****) :: p a (b -> c) -> p a b -> p a c
  (****) f x = Profunctor.dimap dup (uncurry ($)) (f ***! x)
    where dup y = (y, y)

  -- | Use @\\f g -> (,) 'Control.Applicative.<$>'
  -- 'Data.Profunctor.lmap' fst f 'Control.Applicative.<*>'
  -- 'Data.Profunctor.lmap' snd g@ instead.
  -- @(***!)@ may be deprecated in a future version.
  (***!) :: p a b -> p a' b' -> p (a, a') (b, b')
  f ***! g = (,) `Profunctor.rmap` Profunctor.lmap fst f
                  **** Profunctor.lmap snd g

class SemiproductProfunctor p => ProductProfunctor p where
  -- | Unit for @('***!')@.
  unitP :: p x ()
  unitP = pureP ()

  -- | Analogue to 'pure'.
  pureP :: a -> p x a
  pureP a = Profunctor.dimap (const ()) (const a) unitP

  -- | Analogue to 'Data.Functor.Contravariant.Divisible.conquer'
  -- (from "contravariant"). The 'Monoid' constraint is necessary to
  -- provide a "default" value to emit.
  conquerP :: Monoid x => p a x
  conquerP = pureP mempty
  {-# MINIMAL unitP | pureP #-}

  -- | Deprecated alias for 'unitP'. Will be removed in a future version.
  empty :: p () ()
  empty = unitP

  -- | Deprecated alias for 'pureP'. Will be removed in a future version.
  purePP :: a -> p x a
  purePP = pureP

{-# DEPRECATED empty "use unitP" #-}
{-# DEPRECATED purePP "use pureP"#-}

class Profunctor p => SumProfunctor p where
  -- Morally we should have 'zero :: p Void Void' but I don't think
  -- that would actually be useful
  (+++!) :: p a b -> p a' b' -> p (Either a a') (Either b b')
