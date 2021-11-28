module Data.Profunctor.Product.Class where

import           Data.Profunctor (Profunctor(..))
import           Data.Void (Void, absurd)

-- | A 'SemiproductProfunctor' is a profunctor whose input and output
-- values can be combined with @(,)@. ('***!') makes this most
-- obvious, though ('****') and 'liftP2' are equivalent in power.
--
-- A value of type @p a x@ with an @instance 'SemiproductProfunctor' p@
-- often represents some sort of process for turning @a@s into @x@s
-- that can be "laid out side-by-side" with other similar values of
-- @p@ to form "wider" processes. For example, if I have three such
-- encoders:
--
-- @
-- p :: p a x -- a process for turning as into xs
-- q :: p b y -- a process for turning bs into ys
-- r :: p c z -- a process for turning cs into zs
-- @
--
-- I can then combine them using 'p3' to get:
--
-- @
-- -- a process for turning (a, b, c)s into (x, y, z)s
-- p3 p q r :: p (a, b, c) (x, y, z)
-- @
--
-- You would typically compose 'ProductProfunctor's using
-- 'Profunctor'\'s 'lmap', '<$>' \/ 'fmap', and @Apply@ \/
-- 'Applicative'\'s 'pure' and @\<.\>@ \/ @('<*>')@.
--
-- You can often write these instances mechancially:
--
-- @
-- instance SemiproductProfunctor P where
--   (****) = (\<.\>) -- If you have `instance Apply (P a)`
--   (****) = ('<*>') -- If you have `instance 'Applicative' (P a)`
-- @
--
-- Laws:
--
--  * @('***!')@ is associative up to tuple rearrangement.
--
--  * If @p@ is also a 'SemisumProfunctor', @('***!')@ should
--    distribute over @('+++!')@ up to tuple/@Either@ rearrangement.
class Profunctor p => SemiproductProfunctor p where
  (***!) :: p a c -> p b d -> p (a, b) (c, d)
  p ***! q = liftP2 (,) (lmap fst p) (lmap snd q)

  -- | Analogue to @('<*>')@.
  (****) :: p x (a -> b) -> p x a -> p x b
  p **** q = dimap (\x -> (x, x)) (uncurry ($)) $ p ***! q

  -- | Analogue to 'liftA2'.
  liftP2 :: (a -> b -> c) -> p x a -> p x b -> p x c
  liftP2 f p q = rmap f p **** q

  -- | Analogue to @divise@ (from "semigroupoids") or
  -- 'Data.Functor.Contravariant.Divisible.decide' (from
  -- "contravariant"). The 'Semigroup' constraint is necessary to
  -- combine the "output" values.
  --
  -- Why didn't we need a constraint when writing the analogues to
  -- 'Applicative' operations? In the absence of linear types, there
  -- are only trivial comonoids; we can always produce a function to
  -- "duplicate" the input.
  diviseP :: Semigroup x => (a -> (b,c)) -> p b x -> p c x -> p a x
  diviseP f p q = dimap f (uncurry (<>)) $ p ***! q
  {-# MINIMAL (***!) | (****) | liftP2 #-}

-- | Analogue to @divised@ from "semigroupoids" or
-- 'Data.Functor.Contravariant.Divisible.divided' from
-- "contravariant".
divisedP ::
  (SemiproductProfunctor p, Semigroup x) => p a x -> p b x -> p (a, b) x
divisedP = diviseP id

-- | 'SemiproductProfunctor's with a unit.
--
-- If you have an 'Applicative' instance for @P a@, you can write this
-- instance mechanically:
--
-- @
-- instance ProductProfunctor p where
--   pureP = pure
-- @
--
-- Law: @unitP@ is an identity for @('***!')@, up to tuple
-- rearrangement.
class SemiproductProfunctor p => ProductProfunctor p where
  -- | Unit for @('***!')@.
  unitP :: p () ()
  unitP = pureP ()

  -- | Analogue to 'pure'.
  pureP :: a -> p x a
  pureP a = dimap (const ()) (const a) unitP

  -- | Analogue to 'Data.Functor.Contravariant.Divisible.conquer'
  -- (from "contravariant"). The 'Monoid' constraint is necessary to
  -- provide a "default" value to emit.
  --
  -- Unit for 'diviseP'.
  conquerP :: Monoid x => p a x
  conquerP = pureP mempty
  {-# MINIMAL unitP | pureP #-}

-- | As redundant as 'Data.Functor.Contravariant.Divisible.conquered'
-- from "contravariant", but also provided for symmetry.
conqueredP :: (ProductProfunctor p, Monoid x) => p () x
conqueredP = conquerP

-- | A 'SemisumProfunctor' is a profunctor whose input and output
-- values can be combined with 'Either'.
--
-- You can often write these instances mechanically:
--
-- @
-- instance SemisumProfunctor P where
--   decideP f p q = 'Data.Bifunctor.Flip.runFlip' $ decide f ('Data.Bifunctor.Flip.Flip' p) ('Data.Bifunctor.Flip.Flip' q) -- If you have `instance Decide (Flip P a)`
--   decideP f p q = 'Data.Bifunctor.Flip.runFlip' $ 'Data.Functor.Contravariant.Divisible.choose' f ('Data.Bifunctor.Flip.Flip' p) ('Data.Bifunctor.Flip.Flip' q) -- If you have `instance 'Data.Functor.Contravariant.Divisible.Decidable' (Flip P a)`
-- @
--
-- Laws:
--
--  * @('+++!')@ is associative up to 'Either' rearrangement.
--
--  * If @p@ is also a 'SemiproductProfunctor', @('***!')@ should
--    distribute over @('+++!')@ up to tuple/'Either' rearrangement.
--
-- === Where is the 'Control.Applicative.Alternative' analogue?
--
-- It is possible to write a version of @('Control.Applicative.<|>')@
-- that uses 'Either's:
--
-- @
-- alt :: 'Control.Applicative.Alternative' f => f a -> f b -> f ('Either' a b)
-- alt f g = Left \<$\> f 'Control.Applicative.<|>' Right \<$\> g
-- @
--
-- From this, you might expect 'SemisumProfunctor' to contain an
-- analogue to @('Control.Applicative.<|>')@ like this:
--
-- @
-- (\<|||!\>) :: 'SemisumProfunctor' p => p x a -> p x a -> p x a
-- p \<|||!\> q = dimap _____ (either id id) $ p '+++!' q
--                     ???
-- @
--
-- The type of that hole is @x -> 'Either' x x@, and we cannot choose
-- a sensible default. We also cannot introduce a typeclass like we
-- did with 'diviseP', as this would require us to create a strange
-- comonoid class with 'Either' as the bifunctor:
--
-- @
-- -- Yuck:
-- class ComonoidE w where
--   comappendE :: w -> Either w w
--   comemptyE :: w -> 'Void'
-- @
--
-- There are no @ComonoidE@ instances because we can never define a
-- @comemptyE@: 'Void' is uninhabited. Even if we restrict ourselves
-- to a @Cosemigroup@ class, it seems unlikely that it will have any
-- useful instances, so we do not bother defining an
-- 'Control.Applicative.Alternative'-style interface from a
-- 'SemisumProfunctor'.
class Profunctor p => SemisumProfunctor p where
  (+++!) :: p a c -> p b d -> p (Either a b) (Either c d)
  (+++!) p q = decideP id (rmap Left p) (rmap Right q)

  -- | Analogue to @decide@ (from "semigroupoids") or
  -- 'Data.Functor.Contravariant.Divisible.choose' (from
  -- "contravariant").
  decideP :: (a -> Either b c) -> p b x -> p c x -> p a x
  decideP f p q = dimap f (either id id) $ p +++! q
  {-# MINIMAL (+++!) | decideP #-}

-- | Analogue to @decided@ (from "semigroupoids") or
-- 'Data.Functor.Contravariant.Divisible.chosen' (from
-- "contravariant").
decidedP :: SemisumProfunctor p => p b x -> p c x -> p (Either b c) x
decidedP = decideP id

-- | 'SemisumProfunctor's with a unit.
--
-- You can often write these instances mechanically:
--
-- @
-- instance SumProfunctor p where
--   concludeP f = 'Data.Bifunctor.Flip.runFlip' $ conclude f -- If you have `instance Conclude ('Data.Bifunctor.Flip.Flip' P a)`
--   concludeP f = 'Data.Bifunctor.Flip.runFlip' $ 'Data.Functor.Contravariant.Divisible.lose' f -- If you have `instance 'Data.Functor.Contravariant.Divisible.Decidable' ('Data.Bifunctor.Flip.Flip' P a)`
-- @
--
-- Law: @voidP@ is an identity for @('+++!')@, up to @Either@
-- rearrangement.
class SemisumProfunctor p => SumProfunctor p where
  -- | Unit for @('+++!')@.
  voidP :: p Void Void
  voidP = concludeP id

  -- | Analogue to @conclude@ (from "semigroupoids") or
  -- 'Data.Functor.Contravariant.Divisible.lose' (from
  -- "contravariant").
  --
  -- Unit for 'decideP'.
  concludeP :: (a -> Void) -> p a x
  concludeP f = dimap f absurd voidP
  {-# MINIMAL voidP | concludeP #-}

-- | Analogue to @concluded@ (from "semigroupoids") or
-- 'Data.Functor.Contravariant.Divisible.lost'. Potentially more
-- meaningful than 'concludeP', as it shows that we definitely cannot
-- receive _anything_ on the input side.
concludedP :: SumProfunctor p => p Void x
concludedP = concludeP id
