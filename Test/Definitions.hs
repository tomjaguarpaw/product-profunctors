{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Definitions where

-- We define the data types and generate the TH in a separate module
-- because we want to see explicitly what external names are required
-- to be imported for the TH to work, and keep these external names to
-- a minimum.

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Profunctor.Product.Default (Default, def)
import Data.Profunctor.Product (ProductProfunctor, p2, p3)
import Data.Profunctor (dimap)

data Data2 a b = Data2 a b
data Data3 a b c = Data3 a b c

data Record2 a b = Record2 { a2 :: a, b2 :: b }
data Record3 a b c = Record3 { a3 :: a, b3 :: b, c3 :: c }

$(makeAdaptorAndInstance "pData2" ''Data2)
$(makeAdaptorAndInstance "pData3" ''Data3)
$(makeAdaptorAndInstance "pRecord2" ''Record2)
$(makeAdaptorAndInstance "pRecord3" ''Record3)
