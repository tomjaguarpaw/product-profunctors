{-# LANGUAGE TemplateHaskell #-}
module Data.Profunctor.Product (module Data.Profunctor.Product.Class,
                                module Data.Profunctor.Product.Newtype,
                                module Data.Profunctor.Product) where

import Prelude hiding (id)
import Data.Profunctor (Profunctor, dimap, lmap, WrappedArrow)
import qualified Data.Profunctor as Profunctor
import Data.Functor.Contravariant (Contravariant, contramap)
import Control.Category (id)
import Control.Arrow (Arrow, (***), (<<<), arr, (&&&))
import Control.Applicative (Applicative, liftA2, pure)
import Data.Monoid (Monoid, mempty, (<>))
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

-- | '(****)' is the generalisation of @Applicative@'s @\<*\>@.
--
-- You will probably never need to use this.  @\<*\>@ should be
-- sufficient (if your 'ProductProfunctor' instance has also been given
-- an @Applicative@ instance).
(****) :: ProductProfunctor p => p a (b -> c) -> p a b -> p a c
(****) f x = Profunctor.dimap dup (uncurry ($)) (f ***! x)
  where dup y = (y, y)

-- | '(***$)' is the generalisation of @Applicative@'s @\<$\>@.
--
-- '(***$)' = 'Profunctor.rmap', just like '<$>' = 'fmap'.
--
-- You will probably never need to use this.  @\<$\>@ should be
-- sufficient (if your 'ProductProfunctor' instance has also been given
-- an @Applicative@ instance).
(***$) :: ProductProfunctor p => (b -> c) -> p a b -> p a c
(***$) = Profunctor.rmap

-- | 'purePP' is the generalisation of @Applicative@'s @pure@,
--
-- You will probably never need to use this.  @pure@ should be
-- sufficient (if your 'ProductProfunctor' instance also has an
-- @Applicative@ instance).
purePP :: ProductProfunctor p => b -> p a b
purePP b = Profunctor.dimap (const ()) (const b) empty

defaultEmpty :: Applicative (p ()) => p () ()
defaultEmpty = pure ()

defaultProfunctorProduct :: (Applicative (p (a, a')), Profunctor p)
                         => p a b -> p a' b' -> p (a, a') (b, b')
defaultProfunctorProduct p p' = liftA2 (,) (lmap fst p) (lmap snd p')

defaultPoint :: Monoid (p ()) => p ()
defaultPoint = mempty

instance ProductProfunctor (->) where
  empty  = id
  (***!) = (***)

instance Arrow arr => ProductProfunctor (WrappedArrow arr) where
  empty  = id
  (***!) = (***)

-- { Sum

class Profunctor p => SumProfunctor p where
  -- Morally we should have 'zero :: p Void Void' but I don't think
  -- that would actually be useful
  (+++!) :: p a b -> p a' b' -> p (Either a a') (Either b b')

instance SumProfunctor (->) where
  f +++! g = either (Left . f) (Right . g)

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

{-# DEPRECATED ProductContravariant "Use Data.Functor.Contravariant.Divisible instead" #-}
class Contravariant f => ProductContravariant f where
  point  :: f ()
  (***<) :: f a -> f b -> f (a, b)

{-# DEPRECATED AndArrow "If you really need this, file an issue. It will go soon." #-}
data AndArrow arr z a b = AndArrow { runAndArrow :: arr z b }

instance Arrow arr => Profunctor (AndArrow arr z) where
  dimap _ f (AndArrow g) = AndArrow (arr f <<< g)

instance Arrow arr => ProductProfunctor (AndArrow arr z) where
  empty = AndArrow (arr (const ()))
  (AndArrow f) ***! (AndArrow f') = AndArrow (f &&& f')

{-# DEPRECATED defaultContravariantProduct "defaultContravariantProduct will be removed" #-}
defaultContravariantProduct :: (Contravariant f, Monoid (f (a, b)))
                            => f a -> f b -> f (a, b)
defaultContravariantProduct p p' = contramap fst p <> contramap snd p'

{-# DEPRECATED PPOfContravariant "PPOfContravariant will be removed" #-}
newtype PPOfContravariant f a b = PPOfContravariant (f a)

{-# DEPRECATED unPPOfContravariant "unPPOfContravariant will be removed" #-}
unPPOfContravariant :: PPOfContravariant c a a -> c a
unPPOfContravariant (PPOfContravariant pp) = pp

instance Contravariant f => Profunctor (PPOfContravariant f) where
  dimap f _ (PPOfContravariant p) = PPOfContravariant (contramap f p)

instance ProductContravariant f => ProductProfunctor (PPOfContravariant f) where
  empty = PPOfContravariant point
  PPOfContravariant f ***! PPOfContravariant f' = PPOfContravariant (f ***< f')

-- }
