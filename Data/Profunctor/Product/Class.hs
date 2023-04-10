module Data.Profunctor.Product.Class where

import           Data.Profunctor (Profunctor)
import qualified Data.Profunctor as Profunctor
import           Data.Semigroup (Semigroup, (<>))

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
  (***!) :: p a b -> p a' b' -> p (a, a') (b, b')
  f ***! g = liftP2 (,) (Profunctor.lmap fst f) (Profunctor.lmap snd g)

  -- | '****' is the analogue of @Applicative@'s
  -- 'Control.Applicative.<*>'.
  (****) :: p x (a -> b) -> p x a -> p x b
  (****) f a = Profunctor.dimap dup (uncurry ($)) (f ***! a)
    where dup x = (x, x)

  -- | Analogue to 'Control.Applicative.liftA2'
  liftP2 :: (a -> b -> c) -> p x a -> p x b -> p x c
  liftP2 f p q = Profunctor.rmap f p **** q

  -- | Analogue to @divise@ (from "semigroupoids") or
  -- 'Data.Functor.Contravariant.Divisible.decide' (from
  -- "contravariant"). The 'Semigroup' constraint is necessary to
  -- combine the "output" values.
  --
  -- Why didn't we need a constraint when writing the analogues to
  -- 'Applicative' operations? In the absence of linear types, there
  -- are only trivial comonoids; we can always produce a function to
  -- "duplicate" the input.
  diviseP :: Semigroup x => (a -> (b, c)) -> p b x -> p c x -> p a x
  diviseP f p q = Profunctor.dimap f (uncurry (<>)) $ p ***! q
  {-# MINIMAL (***!) | (****) | liftP2 #-}

-- | Analogue to @divised@ from "semigroupoids" or
-- 'Data.Functor.Contravariant.Divisible.divided' from
-- "contravariant".
divisedP ::
  (SemiproductProfunctor p, Semigroup x) => p a x -> p b x -> p (a, b) x
divisedP = diviseP id

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
