{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Profunctor.Product.Internal.Adaptor where

import           Data.Profunctor         (Profunctor, dimap, lmap)
import           Data.Profunctor.Product (ProductProfunctor, (****), (***$))
import           GHC.Generics            (from, to,
                                          M1(M1), K1(K1), (:*:)((:*:)),
                                          Generic, Rep)

-- * Exported

-- | Generic adaptor.
--
-- @
-- 'genericAdaptor' :: 'ProductProfunctor' p =>
--                   'Adaptor' p (Foo (p a a') (p b b') (p c c'))
-- 'genericAdaptor' :: 'ProductProfunctor' p =>
--                   Foo (p a a') (p b b') (p c c') -> p (Foo a b c) (Foo a' b' c')
-- @
genericAdaptor :: GAdaptable p a => Adaptor p a
genericAdaptor a = dimap from to (gAdaptor (from a))

-- | A type synonym to shorten the signature of an adaptor.
--
-- @
-- 'Adaptor' p (Foo (p a a') (p b b') (p c c'))
-- ~
-- Foo (p a a') (p b b') (p c c') -> p (Foo a b c) (Foo a' b' c')
-- @
type Adaptor p a = a -> p (Unzip 'Fst a) (Unzip 'Snd a)

-- * Implementation

-- | A constraint synonym on generic types for which an adaptor can be
-- defined generically.
type GAdaptable p a =
  ( Generic a, Generic (Unzip 'Fst a), Generic (Unzip 'Snd a)
  , GUnzip 'Fst (Rep a) ~ Rep (Unzip 'Fst a)
  , GUnzip 'Snd (Rep a) ~ Rep (Unzip 'Snd a)
  , GAdaptor p (Rep a)
  )

-- | A flag denoting a type-level field accessor.
data Select = Fst | Snd

-- | A type like
--
-- > T = Foo (p a a') (p b b') (p c c')
--
-- can be unzipped to
--
-- > Unzip 'Fst T = Foo a  b  c
-- > Unzip 'Snd T = Foo a' b' c'
--
-- This defines the type family 'Unzip' with versions of GHC older than 8.0.1.
-- For 8.0.1 and newer versions, 'Unzip' is an independent type family
-- and 'Unzippable' is just an empty class for backwards compatibility.
class Unzippable (a :: k) where
#if __GLASGOW_HASKELL__ < 800
  type Unzip (z :: Select) a :: k
  type Unzip z a = a

instance Unzippable (f :: * -> k') => Unzippable (f a) where
  type Unzip z (f a) = Unzip z f (Project z a)
#else

type family Unzip (z :: Select) (a :: k) :: k where
  Unzip z (f a) = Unzip' z f (Project z a)
  Unzip z a = a

-- | A hack to enable kind-polymorphic recursion.
type family Unzip' (z :: Select) (a :: k) :: k where
  Unzip' z a = Unzip z a
#endif

-- There is a bug in GHC < 8 apparently preventing us from using pure
-- type families. https://ghc.haskell.org/trac/ghc/ticket/11699
-- Defining them as associated types seems to be a valid work around.

-- | A type @p a b@ can be seen as a type-level pair @'(a, b)@.
class TypePair a where
  -- | This type synonym extracts a component, @a@ or @b@,
  -- of that pair @p a b@.
  type Project (z :: Select) a

instance forall (p :: * -> * -> *) a b. TypePair (p a b) where
  type Project 'Fst (p a b) = a
  type Project 'Snd (p a b) = b

-- | Unzips the types of fields of a record.
--
-- >             T = (M1 _ _ (K1 _ (p c1 c2))) :*: (M1 _ _ (K1 _ (p d1 d2)))
-- > GUnzip 'Fst T = (M1 _ _ (K1 _    c1    )) :*: (M1 _ _ (K1 _    d1    ))
-- > GUnzip 'Snd T = (M1 _ _ (K1 _       c2 )) :*: (M1 _ _ (K1 _       d2 ))
type family GUnzip (z :: Select) (f :: * -> *) :: * -> *
type instance GUnzip z (f :*: g) = GUnzip z f :*: GUnzip z g
type instance GUnzip z (K1 i c) = K1 i (Project z c)
type instance GUnzip z (M1 i c f) = M1 i c (GUnzip z f)

-- | Adaptors over generic representations of types.
class Profunctor p => GAdaptor p f | f -> p where
  gAdaptor :: f a -> p (GUnzip 'Fst f a) (GUnzip 'Snd f a)

instance
  (ProductProfunctor p, GAdaptor p f, GAdaptor p g)
  => GAdaptor p (f :*: g) where
  gAdaptor (f :*: g) = (:*:)
    ***$ lmap pfst (gAdaptor f)
    **** lmap psnd (gAdaptor g)
    where pfst (f' :*: _) = f'
          psnd (_ :*: g') = g'

instance GAdaptor p f => GAdaptor p (M1 i c f) where
  gAdaptor (M1 f) = dimap
    (\(M1 f') -> f')
    (\f' -> M1 f')
    (gAdaptor f)

instance Profunctor p => GAdaptor p (K1 i (p a b)) where
  gAdaptor (K1 c) = dimap
    (\(K1 c') -> c')
    (\c' -> K1 c')
    c
