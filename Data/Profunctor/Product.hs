{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Profunctor.Product (module Data.Profunctor.Product.Class,
                                module Data.Profunctor.Product.Newtype,
                                module Data.Profunctor.Product) where

import Prelude hiding (id)
import Data.Profunctor (Profunctor, lmap, WrappedArrow, Star(..), Costar)
import qualified Data.Profunctor as Profunctor
import Data.Profunctor.Composition (Procompose(..))
import Data.Functor.Contravariant.Divisible (Divisible(..), Decidable, chosen)
import Control.Category (id)
import Control.Arrow (Arrow, (***), ArrowChoice, (+++))
import Control.Applicative (Applicative, liftA2, pure, (<*>), Alternative, (<|>), (<$>))

import Data.Monoid (Monoid, mempty)
import Data.Tagged

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

-- ProductProfunctor and ProductContravariant are potentially
-- redundant type classes.  It seems to me that these are equivalent
-- to Profunctor with Applicative, and Contravariant with Monoid
-- respectively:
--
--    import Data.Profunctor
--    import Control.Applicative hiding (empty)
--    import Data.Functor.Contravariant
--    import Data.Monoid
--
--    empty :: (Applicative (p ())) => p () ()
--    empty = pure ()
--
--    (***!) :: (Applicative (p (a, a')), Profunctor p) =>
--                p a b -> p a' b' -> p (a, a') (b, b')
--    p ***! p' = (,) <$> lmap fst p <*> lmap snd p'
--
--    point :: Monoid (f ()) => f ()
--    point = mempty
--
--    (***<) :: (Monoid (f (a, b)), Contravariant f) =>
--                f a -> f b -> f (a, b)
--    p ***< p' = contramap fst p <> contramap snd p'
--
--
-- The only thing that makes me think that they are not *completely*
-- redundant is that (***!) and (***<) have to be defined
-- polymorphically in the type arguments, whereas if we took the
-- Profunctor+Applicative or Contravariant+Monoid approach we do not
-- have a guarantee that these operations are polymorphic.
--
-- Previously I wanted to replace ProductProfunctor and
-- ProductContravariant entirely.  This proved difficult as it is not
-- possible to expand the class constraints to require Applicative and
-- Monoid respectively.  We can't enforce a constraint 'Applicative (p
-- a)' where 'a' does not appear in the head.  This seems closely
-- related to the above issue of adhoc implementations.
--
-- There is a potential method of working around this issue using the
-- 'constraints' package:
-- stackoverflow.com/questions/12718268/polymorphic-constraint/12718620
--
-- Still, at least we now have default implementations of the class
-- methods, which makes things simpler.

-- | '***$' is the generalisation of @Functor@'s @\<$\>@.
--
-- '***$' = 'Profunctor.rmap', just like '<$>' = 'fmap'.
--
-- (You probably won't need to use this.  @\<$\>@ should be
-- sufficient.)
(***$) :: Profunctor p => (b -> c) -> p a b -> p a c
(***$) = Profunctor.rmap

instance ProductProfunctor (->) where
  purePP = pure
  (****) = (<*>)

instance Arrow arr => ProductProfunctor (WrappedArrow arr) where
  empty  = id
  (***!) = (***)

instance ProductProfunctor Tagged where
  purePP = pure
  (****) = (<*>)

instance Applicative f => ProductProfunctor (Star f) where
  purePP = pure
  (****) = (<*>)

instance Functor f => ProductProfunctor (Costar f) where
  purePP = pure
  (****) = (<*>)

instance (ProductProfunctor p, ProductProfunctor q) => ProductProfunctor (Procompose p q) where
  purePP a = Procompose (purePP a) (purePP ())
  Procompose pf qf **** Procompose pa qa =
    Procompose (lmap fst pf **** lmap snd pa) ((,) ***$ qf **** qa)

instance (Functor f, Applicative g, ProductProfunctor p) => ProductProfunctor (Biff p f g) where
  purePP = Biff . purePP . pure
  Biff abc **** Biff ab = Biff $ (<*>) ***$ abc **** ab

instance Applicative f => ProductProfunctor (Joker f) where
  purePP = Joker . pure
  Joker bc **** Joker b = Joker $ bc <*> b

instance Divisible f => ProductProfunctor (Clown f) where
  purePP _ = Clown conquer
  Clown l **** Clown r = Clown $ divide (\a -> (a, a)) l r

instance (ProductProfunctor p, ProductProfunctor q) => ProductProfunctor (Product p q) where
  purePP a = Pair (purePP a) (purePP a)
  Pair l1 l2 **** Pair r1 r2 = Pair (l1 **** r1) (l2 **** r2)

instance (Applicative f, ProductProfunctor p) => ProductProfunctor (Tannen f p) where
  purePP = Tannen . pure . purePP
  Tannen f **** Tannen a = Tannen $ liftA2 (****) f a

-- { Sum

instance SumProfunctor (->) where
  f +++! g = either (Left . f) (Right . g)

instance ArrowChoice arr => SumProfunctor (WrappedArrow arr) where
  (+++!) = (+++)

instance Applicative f => SumProfunctor (Star f) where
  Star f +++! Star g = Star $ either (fmap Left . f) (fmap Right . g)

instance (SumProfunctor p, SumProfunctor q) => SumProfunctor (Procompose p q) where
  Procompose pa qa +++! Procompose pb qb = Procompose (pa +++! pb) (qa +++! qb)

instance Alternative f => SumProfunctor (Joker f) where
  Joker f +++! Joker g = Joker $ Left <$> f <|> Right <$> g

instance Decidable f => SumProfunctor (Clown f) where
  Clown f +++! Clown g = Clown $ chosen f g

instance (SumProfunctor p, SumProfunctor q) => SumProfunctor (Product p q) where
  Pair l1 l2 +++! Pair r1 r2 = Pair (l1 +++! r1) (l2 +++! r2)

instance (Applicative f, SumProfunctor p) => SumProfunctor (Tannen f p) where
  Tannen l +++! Tannen r = Tannen $ liftA2 (+++!) l r

-- | A generalisation of @map :: (a -> b) -> [a] -> [b]@.  It is also,
-- in spirit, a generalisation of @traverse :: (a -> f b) -> [a] -> f
-- [b]@, but the types need to be shuffled around a bit to make that
-- work.
list :: (ProductProfunctor p, SumProfunctor p) => p a b -> p [a] [b]
list p = Profunctor.dimap fromList toList (empty +++! (p ***! list p))
  where toList :: Either () (a, [a]) -> [a]
        toList = either (const []) (uncurry (:))
        fromList :: [a] -> Either () (a, [a])
        fromList []     = Left ()
        fromList (a:as) = Right (a, as)

-- SumContravariant would be 'Data.Functor.Contravariant.Decidable'
-- (without the requirement to also be Divisible).

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
