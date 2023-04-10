{-# LANGUAGE ConstraintKinds, DataKinds, DefaultSignatures, FlexibleContexts,
             FlexibleInstances, LambdaCase,
             MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
module Data.Profunctor.Product.Default.Class where

import GHC.Exts (Constraint)
import GHC.Generics

import Data.Profunctor (Profunctor, dimap)
import Data.Profunctor.Product.Class

class Default p a b where
  -- Would rather call it "default", but that's a keyword
  def :: p a b
  default def :: (Profunctor p, Generic a, Generic b, GDefault p (Rep a) (Rep b)) => p a b
  def = gdef

-- | See 'DefaultFields''. But this more general form allows the input and
-- output types to vary a bit.
type DefaultFields p a b = GDefCnstr p (Rep a) (Rep b)

-- | 'Default' constraints on the fields of a 'Generic' datatype.
--
-- For a type like
--
-- > data Foo = Bar { a :: Int, b :: String }
-- >          | Baz Bool
--
-- we get the following constraints
--
-- > DefaultFields' p Foo =
-- >   ( Default p Int Int
-- >   , Default p String String
-- >   , Default p Bool Bool
-- >   )
type DefaultFields' p a = DefaultFields p a a

-- | @'DefaultPConstraints' p a@ expands to the minimal combination of
-- @'Profunctor' p@, @'SemiproductProfunctor' p@, @'SemisumProfunctor' p@ needed to implement
-- the instance @'Default' p a a@ for a 'Generic' datatype @a@.
--
-- > DefaultPConstraints p Foo =
-- >   ( SemiproductProfunctor p      -- because Foo has a constructor Bar with many fields
-- >   , SemisumProfunctor p              -- because Foo has multiple constructors
-- >   )
--
-- > DefaultConstraints p (a, b) =
-- >   ( SemiproductProfunctor p      -- (a, b) has a single constructor with two fields
-- >   )
type DefaultPConstraints p a = GDefPCnstr p (Rep a)

-- | @'DefaultConstraints' p a b@ forms all of the context needed to implement
-- the instance @'Default' p a b@ for 'Generic' types @a@ and @b@.
--
-- This serves to abbreviate the context in instances of 'Default' for
-- parametric types.
type DefaultConstraints p a b = (DefaultPConstraints p a, DefaultFields p a b)

-- | This serves to abbreviate the context in instances of 'Default' for
-- non-parametric types.
type DefaultConstraints' p a = DefaultConstraints p a a

-- | A list of 'Default' constraints.
--
-- > Defaults '[p a a', p b b', p c c'] =
-- >   (Default p a a', Default p b b', Default p c c')
type family Defaults (as :: [*]) :: Constraint
type instance Defaults '[] = ()
type instance Defaults (p a a' ': as) = (Default p a a', Defaults as)

-- * Generic instance for Default

class GDefault p f g where
  type GDefCnstr p f g :: Constraint
  gdef1 :: p (f a) (g a)

instance ProductProfunctor p => GDefault p U1 U1 where
  type GDefCnstr p U1 U1 = ()
  gdef1 = dimap (const ()) (const U1) unitP

instance (Profunctor p, GDefault p f g) => GDefault p (M1 i c f) (M1 i c g) where
  type GDefCnstr p (M1 i c f) (M1 i c g) = GDefCnstr p f g
  gdef1 = dimap unM1 M1 gdef1

instance (Profunctor p, Default p c c') => GDefault p (K1 i c) (K1 i c') where
  type GDefCnstr p (K1 i c) (K1 i c') = Default p c c'
  gdef1 = dimap unK1 K1 def

instance (SemiproductProfunctor p, GDefault p f f', GDefault p g g') => GDefault p (f :*: g) (f' :*: g') where
  type GDefCnstr p (f :*: g) (f' :*: g') = (GDefCnstr p f f', GDefCnstr p g g')
  gdef1 = dimap (\(x :*: y) -> (x, y)) (uncurry (:*:)) $ gdef1 ***! gdef1

instance (SemisumProfunctor p, GDefault p f f', GDefault p g g') => GDefault p (f :+: g) (f' :+: g') where
  type GDefCnstr p (f :+: g) (f' :+: g') = (GDefCnstr p f f', GDefCnstr p g g')
  gdef1 = dimap sumToEither eitherToSum $ gdef1 +++! gdef1
    where
      eitherToSum = \case
        Left  x -> L1 x
        Right x -> R1 x
      sumToEither = \case
        L1 x -> Left  x
        R1 x -> Right x

type family GDefPCnstr (p :: * -> * -> *) (f :: * -> *) :: Constraint
type instance GDefPCnstr p U1 = SemiproductProfunctor p
type instance GDefPCnstr p (M1 i c f) = GDefPCnstr p f
type instance GDefPCnstr p (K1 i c) = Profunctor p
type instance GDefPCnstr p (f :*: g) = SemiproductProfunctor p
type instance GDefPCnstr p (f :+: g) = (SemisumProfunctor p, GDefPCnstr p f, GDefPCnstr p g)

gdef :: (Profunctor p, Generic a, Generic b, GDefault p (Rep a) (Rep b)) => p a b
gdef = dimap from to gdef1
