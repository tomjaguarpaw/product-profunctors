{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Definitions where

import GHC.Generics (Generic)

-- We define the data types and generate the TH in a separate module
-- because we want to ensure that no external names are required to be
-- imported.

import Data.Profunctor
import Data.Profunctor.Product
import Data.Profunctor.Product.Adaptor (Unzippable)
import Data.Profunctor.Product.Default (Default, def)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance, makeAdaptorAndInstance',
                                  makeAdaptorAndInstanceInferrable)

data Data2 a b = Data2 a b
  deriving Generic
data Data3 a b c = Data3 a b c
  deriving Generic

data Record2 a b = Record2 { a2 :: a, b2 :: b }
  deriving Generic
data Record3 a b c = Record3 { a3 :: a, b3 :: b, c3 :: c }
  deriving Generic

data RecordDefaultName x y = RecordDefaultName { x :: x, y :: y }
  deriving Generic

data DataGeneric a b c = DataGeneric a b c deriving Generic
data RecordGeneric a b c = RecordGeneric { a4 :: a, b4 :: b, c4 :: c } deriving Generic
data SumGeneric a b = SumL a | SumR b deriving Generic
data ProductAndSumGeneric a b c = PSumL a | PSumR b c deriving Generic

$(makeAdaptorAndInstance "pData2" ''Data2)
$(makeAdaptorAndInstance "pData3" ''Data3)
$(makeAdaptorAndInstance "pRecord2" ''Record2)
$(makeAdaptorAndInstance "pRecord3" ''Record3)
$(makeAdaptorAndInstance' ''RecordDefaultName)

instance Unzippable Data2
instance Unzippable Data3

instance (ProductProfunctor p, Default p a a', Default p b b', Default p c c')
      => Default p (DataGeneric a b c) (DataGeneric a' b' c')

instance (ProductProfunctor p, Default p a a', Default p b b', Default p c c')
      => Default p (RecordGeneric a b c) (RecordGeneric a' b' c')

instance (SumProfunctor p, Default p a a', Default p b b')
      => Default p (SumGeneric a b) (SumGeneric a' b')

instance (ProductProfunctor p, SumProfunctor p, Default p a a', Default p b b', Default p c c')
      => Default p (ProductAndSumGeneric a b c) (ProductAndSumGeneric a' b' c')

data Data2Inferrable a b = Data2Inferrable a b
data Record2Inferrable a b = Record2Inferrable { a2I :: a, b2I :: b } deriving Show

$(makeAdaptorAndInstanceInferrable "pData2Inferrable" ''Data2Inferrable)
$(makeAdaptorAndInstanceInferrable "pRecord2Inferrable" ''Record2Inferrable)

newtype Arrow a b = Arrow { unArrow :: a -> b }

instance Profunctor Arrow where
  dimap f g = Arrow . dimap f g . unArrow

instance ProductProfunctor Arrow where
  purePP = Arrow . purePP
  f **** g = Arrow (unArrow f **** unArrow g)

data Unit = Unit

class Pointed a where
  point :: a

instance Pointed Unit where
  point = Unit

instance (Pointed a, Pointed b) => Pointed (Data2Inferrable a b) where
  point = Data2Inferrable point point

instance (Pointed a, Pointed b) => Pointed (Record2Inferrable a b) where
  point = Record2Inferrable point point

instance {-# INCOHERENT #-} a ~ Unit => Default Arrow Unit a where
  def = Arrow id

instance {-# INCOHERENT #-} a ~ Unit => Default Arrow a Unit where
  def = Arrow id
