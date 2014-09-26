module Data.Profunctor.Product where

import Prelude hiding (id)
import Data.Profunctor (Profunctor, dimap, lmap, WrappedArrow)
import Data.Functor.Contravariant (Contravariant, contramap)
-- vv TODO: don't want to have to import all those explicitly.  What to do?
import Data.Profunctor.Product.Flatten
-- vv and these
import Data.Profunctor.Product.Tuples
import Control.Category (id)
import Control.Arrow (Arrow, (***), (<<<), arr, (&&&))
import Control.Applicative (Applicative, liftA2, pure)
import Data.Monoid (Monoid, mempty, (<>))

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

class Profunctor p => ProductProfunctor p where
  empty :: p () ()
  (***!) :: p a b -> p a' b' -> p (a, a') (b, b')

class Contravariant f => ProductContravariant f where
  point :: f ()
  (***<) :: f a -> f b -> f (a, b)

defaultEmpty :: Applicative (p ()) => p () ()
defaultEmpty = pure ()

defaultProfunctorProduct :: (Applicative (p (a, a')), Profunctor p)
                  => p a b -> p a' b' -> p (a, a') (b, b')
defaultProfunctorProduct p p' = liftA2 (,) (lmap fst p) (lmap snd p')

defaultPoint :: Monoid (p ()) => p ()
defaultPoint = mempty

defaultContravariantProduct :: (Contravariant f, Monoid (f (a, b)))
                               => f a -> f b -> f (a, b)
defaultContravariantProduct p p' = contramap fst p <> contramap snd p'

newtype PPOfContravariant f a b = PPOfContravariant (f a)

unPPOfContravariant :: PPOfContravariant c a a -> c a
unPPOfContravariant (PPOfContravariant pp) = pp

instance Contravariant f => Profunctor (PPOfContravariant f) where
  dimap f _ (PPOfContravariant p) = PPOfContravariant (contramap f p)

instance ProductContravariant f => ProductProfunctor (PPOfContravariant f) where
  empty = PPOfContravariant point
  PPOfContravariant f ***! PPOfContravariant f' = PPOfContravariant (f ***< f')

instance ProductProfunctor (->) where
  empty = id
  (***!) = (***)

instance Arrow arr => ProductProfunctor (WrappedArrow arr) where
  empty = id
  (***!) = (***)

data AndArrow arr z a b = AndArrow { runAndArrow :: arr z b }

instance Arrow arr => Profunctor (AndArrow arr z) where
  dimap _ f (AndArrow g) = AndArrow (arr f <<< g)

instance Arrow arr => ProductProfunctor (AndArrow arr z) where
  empty = AndArrow (arr (const ()))
  (AndArrow f) ***! (AndArrow f') = AndArrow (f &&& f')

pT0 :: ProductProfunctor p => T0 -> p T0 T0
pT0 = const empty

pT1 :: ProductProfunctor p => T1 (p a1 b1) -> p (T1 a1) (T1 b1)
pT1 = id

pT2 :: ProductProfunctor p => T2 (p a1 b1) (p a2 b2) -> p (T2 a1 a2) (T2 b1 b2)
pT2 = uncurry (***!)

chain :: ProductProfunctor p => (t -> p a2 b2) -> (p a1 b1, t)
      -> p (a1, a2) (b1, b2)
chain rest (a, as) = pT2 (a, rest as)

pT3 :: ProductProfunctor p => T3 (p a1 b1) (p a2 b2) (p a3 b3)
       -> p (T3 a1 a2 a3) (T3 b1 b2 b3)
pT3 = chain pT2

pT4 :: ProductProfunctor p => T4 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
       -> p (T4 a1 a2 a3 a4) (T4 b1 b2 b3 b4)
pT4 = chain pT3

pT5 :: ProductProfunctor p => T5 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                 (p a5 b5)
       -> p (T5 a1 a2 a3 a4 a5) (T5 b1 b2 b3 b4 b5)
pT5 = chain pT4

pT6 :: ProductProfunctor p => T6 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                 (p a5 b5) (p a6 b6)
       -> p (T6 a1 a2 a3 a4 a5 a6) (T6 b1 b2 b3 b4 b5 b6)
pT6 = chain pT5

pT7 :: ProductProfunctor p => T7 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                 (p a5 b5) (p a6 b6) (p a7 b7)
       -> p (T7 a1 a2 a3 a4 a5 a6 a7) (T7 b1 b2 b3 b4 b5 b6 b7)
pT7 = chain pT6

pT8 :: ProductProfunctor p => T8 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                 (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8)
       -> p (T8 a1 a2 a3 a4 a5 a6 a7 a8) (T8 b1 b2 b3 b4 b5 b6 b7 b8)
pT8 = chain pT7

pT9 :: ProductProfunctor p => T9 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                 (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8)
                                 (p a9 b9)
       -> p (T9 a1 a2 a3 a4 a5 a6 a7 a8 a9)
            (T9 b1 b2 b3 b4 b5 b6 b7 b8 b9)
pT9 = chain pT8

pT10 :: ProductProfunctor p => T10 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                   (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8)
                                   (p a9 b9) (p a10 b10)
       -> p (T10 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
            (T10 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10)
pT10 = chain pT9

pT11 :: ProductProfunctor p => T11 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                   (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8)
                                   (p a9 b9) (p a10 b10) (p a11 b11)
       -> p (T11 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
            (T11 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11)
pT11 = chain pT10

pT12 :: ProductProfunctor p => T12 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                   (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8)
                                   (p a9 b9) (p a10 b10) (p a11 b11)
                                   (p a12 b12)
       -> p (T12 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
            (T12 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12)
pT12 = chain pT11

pT13 :: ProductProfunctor p => T13 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                   (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8)
                                   (p a9 b9) (p a10 b10) (p a11 b11)
                                   (p a12 b12) (p a13 b13)
       -> p (T13 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13)
            (T13 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13)
pT13 = chain pT12

pT14 :: ProductProfunctor p => T14 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                   (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8)
                                   (p a9 b9) (p a10 b10) (p a11 b11)
                                   (p a12 b12) (p a13 b13) (p a14 b14)
       -> p (T14 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)
            (T14 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14)
pT14 = chain pT13

pT15 :: ProductProfunctor p => T15 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                   (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8)
                                   (p a9 b9) (p a10 b10) (p a11 b11)
                                   (p a12 b12) (p a13 b13) (p a14 b14)
                                   (p a15 b15)
       -> p (T15 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
            (T15 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
pT15 = chain pT14

pT16 :: ProductProfunctor p => T16 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                   (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8)
                                   (p a9 b9) (p a10 b10) (p a11 b11)
                                   (p a12 b12) (p a13 b13) (p a14 b14)
                                   (p a15 b15) (p a16 b16)
       -> p (T16 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16)
            (T16 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16)
pT16 = chain pT15

pT17 :: ProductProfunctor p => T17 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                   (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8)
                                   (p a9 b9) (p a10 b10) (p a11 b11)
                                   (p a12 b12) (p a13 b13) (p a14 b14)
                                   (p a15 b15) (p a16 b16) (p a17 b17)
       -> p (T17 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17)
            (T17 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17)
pT17 = chain pT16

pT18 :: ProductProfunctor p => T18 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                   (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8)
                                   (p a9 b9) (p a10 b10) (p a11 b11)
                                   (p a12 b12) (p a13 b13) (p a14 b14)
                                   (p a15 b15) (p a16 b16) (p a17 b17)
                                   (p a18 b18)
       -> p (T18 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18)
            (T18 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18)
pT18 = chain pT17

convert :: Profunctor p => (a2 -> a1) -> (tp -> tTp) -> (b1 -> b2)
                           -> (tTp -> p a1 b1)
                           -> tp -> p a2 b2
convert u u' f c = dimap u f . c . u'

p0 :: ProductProfunctor p => () -> p () ()
p0 = convert unflatten0 unflatten0 flatten0 pT0

p1 :: ProductProfunctor p => p a1 b1 -> p a1 b1
p1 = convert unflatten1 unflatten1 flatten1 pT1

p2 :: ProductProfunctor p => (p a1 b1, p a2 b2) -> p (a1, a2) (b1, b2)
p2 = convert unflatten2 unflatten2 flatten2 pT2

p3 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3)
      -> p (a1, a2, a3) (b1, b2, b3)
p3 = convert unflatten3 unflatten3 flatten3 pT3

p4 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3, p a4 b4)
      -> p (a1, a2, a3, a4) (b1, b2, b3, b4)
p4 = convert unflatten4 unflatten4 flatten4 pT4

p5 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3, p a4 b4,
                              p a5 b5)
      -> p (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
p5 = convert unflatten5 unflatten5 flatten5 pT5

p6 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3, p a4 b4,
                              p a5 b5, p a6 b6)
      -> p (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
p6 = convert unflatten6 unflatten6 flatten6 pT6

p7 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3, p a4 b4,
                              p a5 b5, p a6 b6, p a7 b7)
      -> p (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
p7 = convert unflatten7 unflatten7 flatten7 pT7

p8 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3, p a4 b4,
                              p a5 b5, p a6 b6, p a7 b7, p a8 b8)
      -> p (a1, a2, a3, a4, a5, a6, a7, a8) (b1, b2, b3, b4, b5, b6, b7, b8)
p8 = convert unflatten8 unflatten8 flatten8 pT8

p9 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3, p a4 b4,
                              p a5 b5, p a6 b6, p a7 b7, p a8 b8,
                              p a9 b9)
      -> p (a1, a2, a3, a4, a5, a6, a7, a8, a9)
           (b1, b2, b3, b4, b5, b6, b7, b8, b9)
p9 = convert unflatten9 unflatten9 flatten9 pT9

p10 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3, p a4 b4,
                              p a5 b5, p a6 b6, p a7 b7, p a8 b8,
                              p a9 b9, p a10 b10)
      -> p (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
           (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)
p10 = convert unflatten10 unflatten10 flatten10 pT10

p11 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3, p a4 b4,
                              p a5 b5, p a6 b6, p a7 b7, p a8 b8,
                              p a9 b9, p a10 b10, p a11 b11)
      -> p (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
           (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11)
p11 = convert unflatten11 unflatten11 flatten11 pT11

p12 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3, p a4 b4,
                              p a5 b5, p a6 b6, p a7 b7, p a8 b8,
                              p a9 b9, p a10 b10, p a11 b11, p a12 b12)
      -> p (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
           (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12)
p12 = convert unflatten12 unflatten12 flatten12 pT12

p13 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3, p a4 b4,
                              p a5 b5, p a6 b6, p a7 b7, p a8 b8,
                              p a9 b9, p a10 b10, p a11 b11, p a12 b12,
                              p a13 b13)
      -> p (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
           (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13)
p13 = convert unflatten13 unflatten13 flatten13 pT13

p14 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3, p a4 b4,
                              p a5 b5, p a6 b6, p a7 b7, p a8 b8,
                              p a9 b9, p a10 b10, p a11 b11, p a12 b12,
                              p a13 b13, p a14 b14)
      -> p (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
           (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14)
p14 = convert unflatten14 unflatten14 flatten14 pT14

p15 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3, p a4 b4,
                              p a5 b5, p a6 b6, p a7 b7, p a8 b8,
                              p a9 b9, p a10 b10, p a11 b11, p a12 b12,
                              p a13 b13, p a14 b14, p a15 b15)
      -> p (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
           (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15)
p15 = convert unflatten15 unflatten15 flatten15 pT15

p16 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3, p a4 b4,
                              p a5 b5, p a6 b6, p a7 b7, p a8 b8,
                              p a9 b9, p a10 b10, p a11 b11, p a12 b12,
                              p a13 b13, p a14 b14, p a15 b15, p a16 b16)
      -> p (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
           (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16)
p16 = convert unflatten16 unflatten16 flatten16 pT16

p17 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3, p a4 b4,
                              p a5 b5, p a6 b6, p a7 b7, p a8 b8,
                              p a9 b9, p a10 b10, p a11 b11, p a12 b12,
                              p a13 b13, p a14 b14, p a15 b15, p a16 b16, p a17 b17)
      -> p (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
           (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17)
p17 = convert unflatten17 unflatten17 flatten17 pT17

p18 :: ProductProfunctor p => (p a1 b1, p a2 b2, p a3 b3, p a4 b4,
                              p a5 b5, p a6 b6, p a7 b7, p a8 b8,
                              p a9 b9, p a10 b10, p a11 b11, p a12 b12,
                              p a13 b13, p a14 b14, p a15 b15, p a16 b16,
                              p a17 b17, p a18 b18)
      -> p (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
           (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18)
p18 = convert unflatten18 unflatten18 flatten18 pT18
