{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Definitions where

-- We define the data types and generate the TH in a separate module
-- because we want to ensure that no external names are required to be
-- imported.

import Data.Profunctor.Product.TH (makeAdaptorAndInstance, makeAdaptorAndInstance')

data Data2 a b = Data2 a b
data Data3 a b c = Data3 a b c

data Record2 a b = Record2 { a2 :: a, b2 :: b }
data Record3 a b c = Record3 { a3 :: a, b3 :: b, c3 :: c }

data RecordDefaultName x y = RecordDefaultName { x :: x, y :: y }

$(makeAdaptorAndInstance "pData2" ''Data2)
$(makeAdaptorAndInstance "pData3" ''Data3)
$(makeAdaptorAndInstance "pRecord2" ''Record2)
$(makeAdaptorAndInstance "pRecord3" ''Record3)
makeAdaptorAndInstance' ''RecordDefaultName
