{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The classes in this module provide "profunctorial" analogues to
-- the operations from the 'Applicative' (@Apply@),
-- 'Data.Functor.Contravariant.Divisible.Divisible' (@Divise@) and
-- 'Data.Functor.Contravariant.Divisible.Decidable' (@Conclude@) type
-- classes:
--
-- @
-- ('<*>')  :: 'Applicative'           f => f   (a -> b) -> f   a -> f   b
-- ('****') :: 'SemiproductProfunctor' p => p x (a -> b) -> p x a -> p x b
--
-- 'Control.Applicative.liftA2' :: 'Applicative'           f => (a -> b -> c) -> f   a -> f   b -> f   c
-- 'liftP2' :: 'SemiproductProfunctor' p => (a -> b -> c) -> p x a -> p x b -> p x c
--
-- pure  :: 'Applicative'           f => a -> f   a
-- 'pureP' :: 'SemiproductProfunctor' p => a -> p x a
--
-- divide  ::                           'Data.Functor.Contravariant.Divisible.Divisible' f  => (a -> (b, c)) -> f b   -> f c   -> f a -- From contravariant
-- divise  ::                              Divise f  => (a -> (b, c)) -> f b   -> f c   -> f a -- From semigroupoids
-- 'diviseP' :: ('Semigroup' x, 'SemiproductProfunctor' p) => (a -> (b, c)) -> p a x -> p b x -> p c x
--
-- conquer ::                      'Data.Functor.Contravariant.Divisible.Decidable' f => f a -- From contravariant
-- 'conquerP' :: ('Monoid' x, 'ProductProfunctor' p) => p a x
--
-- choose  :: 'Data.Functor.Contravariant.Divisible.Decidable'         f => (a -> 'Either' b c) -> f b   -> f c   -> f a -- From contravariant
-- decide  :: Decide            f => (a -> 'Either' b c) -> f b   -> f c   -> f a -- From semigroupoids
-- 'decideP' :: 'SemisumProfunctor' p => (a -> 'Either' b c) -> p b x -> p c x -> p a x
--
-- lose      :: 'Data.Functor.Contravariant.Divisible.Decidable'     f => (a -> 'Void') -> f a -- From contravariant
-- conclude  :: Conclude      f => (a -> 'Void') -> f a -- From semigroupoids
-- 'concludeP' :: 'SumProfunctor' p => (a -> 'Void') -> p a x
-- @
--
-- The @(Semi){Sum,Product}Profunctor@ classes also provide more
-- primitive operations using @Either@ and @(,)@. These can be very
-- useful with the @<https://hackage.haskell.org/package/generics-eot generics-eot>@
-- package, which can automatically convert data types that have a
-- 'Generic' instance into Eithers-of-Tuples.
module Data.Profunctor.Product (module Data.Profunctor.Product.Class,
                                module Data.Profunctor.Product.Newtype,
                                module Data.Profunctor.Product) where

import Prelude hiding (id)
import Data.Profunctor (Profunctor, lmap, dimap, WrappedArrow, Star(Star), Costar)
import qualified Data.Profunctor as Profunctor
import Data.Profunctor.Composition (Procompose(Procompose))
import Data.Functor.Contravariant.Divisible (Divisible, divide, conquer, Decidable, chosen, lost)
import Control.Category (id)
import Control.Arrow (Arrow, arr, (***), ArrowChoice, (+++))
import Control.Applicative (Applicative, liftA2, pure, (<*>), Alternative, empty, (<|>), (<$>))

import Data.Tagged
import Data.Void (absurd)

import Data.Bifunctor.Biff
import Data.Bifunctor.Clown
import Data.Bifunctor.Joker
import Data.Bifunctor.Product
import Data.Bifunctor.Tannen

import Data.Profunctor.Product.Newtype

import Data.Profunctor.Product.Class
import Data.Profunctor.Product.Flatten
import Data.Profunctor.Product.Tuples
import Data.Profunctor.Product.Tuples.TH (pTns, maxTupleSize, pNs)

-- | '***$' is the generalisation of 'Functor''s @\<$\>@.
--
-- '***$' = 'Profunctor.rmap', just like '<$>' = 'fmap'.
--
-- (You probably won't need to use this.  @\<$\>@ should be
-- sufficient.)
(***$) :: Profunctor p => (b -> c) -> p a b -> p a c
(***$) = Profunctor.rmap

-- { Product

instance SemiproductProfunctor (->) where
  (****) = (<*>)

instance ProductProfunctor (->) where
  pureP = pure

instance Arrow arr => SemiproductProfunctor (WrappedArrow arr) where
  (***!) = (***)

instance Arrow arr => ProductProfunctor (WrappedArrow arr) where
  unitP = arr id

instance SemiproductProfunctor Tagged where
  (****) = (<*>)

instance ProductProfunctor Tagged where
  pureP = pure

-- Should this use Apply?
instance Applicative f => SemiproductProfunctor (Star f) where
  (****) = (<*>)

instance Applicative f => ProductProfunctor (Star f) where
  pureP = pure

instance Functor f => SemiproductProfunctor (Costar f) where
  (****) = (<*>)

instance Functor f => ProductProfunctor (Costar f) where
  pureP = pure

instance (SemiproductProfunctor p, SemiproductProfunctor q) => SemiproductProfunctor (Procompose p q) where
  Procompose pf qf **** Procompose pa qa =
    Procompose (lmap fst pf **** lmap snd pa) (liftP2 (,) qf qa)

instance (ProductProfunctor p, ProductProfunctor q) => ProductProfunctor (Procompose p q) where
  pureP a = Procompose (pureP a) (pureP ())

instance (Functor f, Applicative g, SemiproductProfunctor p) => SemiproductProfunctor (Biff p f g) where
  Biff abc **** Biff ab = Biff $ liftP2 (<*>) abc ab

instance (Functor f, Applicative g, ProductProfunctor p) => ProductProfunctor (Biff p f g) where
  pureP = Biff . pureP . pure

-- Should this use Apply?
instance Applicative f => SemiproductProfunctor (Joker f) where
  Joker bc **** Joker b = Joker $ bc <*> b

instance Applicative f => ProductProfunctor (Joker f) where
  pureP = Joker . pure

-- Should this use Devise?
instance Divisible f => SemiproductProfunctor (Clown f) where
  Clown l **** Clown r = Clown $ divide (\a -> (a, a)) l r

instance Divisible f => ProductProfunctor (Clown f) where
  pureP _ = Clown conquer

instance (SemiproductProfunctor p, SemiproductProfunctor q) => SemiproductProfunctor (Product p q) where
  Pair l1 l2 **** Pair r1 r2 = Pair (l1 **** r1) (l2 **** r2)

instance (ProductProfunctor p, ProductProfunctor q) => ProductProfunctor (Product p q) where
  pureP a = Pair (pureP a) (pureP a)

-- Should this use Apply?
instance (Applicative f, SemiproductProfunctor p) => SemiproductProfunctor (Tannen f p) where
  Tannen f **** Tannen a = Tannen $ liftA2 (****) f a

instance (Applicative f, ProductProfunctor p) => ProductProfunctor (Tannen f p) where
  pureP = Tannen . pure . pureP

-- }

-- { Sum

instance SemisumProfunctor (->) where
  f +++! g = either (Left . f) (Right . g)

instance SumProfunctor (->) where
  voidP = id

instance ArrowChoice arr => SemisumProfunctor (WrappedArrow arr) where
  (+++!) = (+++)

instance ArrowChoice arr => SumProfunctor (WrappedArrow arr) where
  voidP = arr id

instance Functor f => SemisumProfunctor (Star f) where
  Star f +++! Star g = Star $ either (fmap Left . f) (fmap Right . g)

instance Applicative f => SumProfunctor (Star f) where
  voidP = Star pure

instance (SemisumProfunctor p, SemisumProfunctor q) => SemisumProfunctor (Procompose p q) where
  Procompose pa qa +++! Procompose pb qb = Procompose (pa +++! pb) (qa +++! qb)

instance (SumProfunctor p, SumProfunctor q) => SumProfunctor (Procompose p q) where
  voidP = Procompose voidP voidP

-- Should this use Alt?
instance Alternative f => SemisumProfunctor (Joker f) where
  Joker f +++! Joker g = Joker $ Left <$> f <|> Right <$> g

instance Alternative f => SumProfunctor (Joker f) where
  voidP = Joker $ absurd <$> empty

-- Should this use Decide?
instance Decidable f => SemisumProfunctor (Clown f) where
  Clown f +++! Clown g = Clown $ chosen f g

-- Should this use Conclude?
instance Decidable f => SumProfunctor (Clown f) where
  voidP = Clown lost

instance (SemisumProfunctor p, SemisumProfunctor q) => SemisumProfunctor (Product p q) where
  Pair l1 l2 +++! Pair r1 r2 = Pair (l1 +++! r1) (l2 +++! r2)

instance (SumProfunctor p, SumProfunctor q) => SumProfunctor (Product p q) where
  voidP = Pair voidP voidP

-- Should this use Apply?
instance (Applicative f, SemisumProfunctor p) => SemisumProfunctor (Tannen f p) where
  Tannen l +++! Tannen r = Tannen $ liftA2 (+++!) l r

instance (Applicative f, SumProfunctor p) => SumProfunctor (Tannen f p) where
  voidP = Tannen $ pure voidP

-- | A generalisation of @map :: (a -> b) -> [a] -> [b]@.  It is also,
-- in spirit, a generalisation of @traverse :: (a -> f b) -> [a] -> f
-- [b]@, but the types need to be shuffled around a bit to make that
-- work.
list :: (ProductProfunctor p, SumProfunctor p) => p a b -> p [a] [b]
list p = Profunctor.dimap fromList toList (unitP +++! (p ***! list p))
  where toList :: Either () (a, [a]) -> [a]
        toList = either (const []) (uncurry (:))
        fromList :: [a] -> Either () (a, [a])
        fromList []     = Left ()
        fromList (a:as) = Right (a, as)

-- }

pTns [0..maxTupleSize]

pNs [0..maxTupleSize]

-- { Deprecated stuff

{-# DEPRECATED defaultEmpty "Use pure () instead" #-}
defaultEmpty :: Applicative (p ()) => p () ()
defaultEmpty = pure ()

{-# DEPRECATED defaultProfunctorProduct "Use \\p p' -> liftA2 (,) (lmap fst p) (lmap snd p') instead" #-}
defaultProfunctorProduct :: (Applicative (p (a, a')), Profunctor p)
                         => p a b -> p a' b' -> p (a, a') (b, b')
defaultProfunctorProduct p p' = liftA2 (,) (lmap fst p) (lmap snd p')

{-# DEPRECATED defaultPoint "Use mempty instead" #-}
defaultPoint :: Monoid (p ()) => p ()
defaultPoint = mempty

-- }
