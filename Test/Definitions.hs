{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Definitions where

-- We define the data types and generate the TH in a separate module
-- because we want to ensure that no external names are required to be
-- imported.

import Data.Profunctor.Product.Adaptor (Unzippable)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance, makeAdaptorAndInstance')
import GHC.Generics

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

$(makeAdaptorAndInstance "pData2" ''Data2)
$(makeAdaptorAndInstance "pData3" ''Data3)
$(makeAdaptorAndInstance "pRecord2" ''Record2)
$(makeAdaptorAndInstance "pRecord3" ''Record3)
makeAdaptorAndInstance' ''RecordDefaultName

instance Unzippable Data2
instance Unzippable Data3
