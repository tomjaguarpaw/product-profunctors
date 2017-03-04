{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE PartialTypeSignatures #-}

module DefinitionsUndecidable where

-- | We define the data types and generate the TH in a separate module
-- because we want to ensure that no external names are required to be
-- imported.
--
-- It's a bit sad that these need UndecidableInstances

import GHC.Generics (Generic)
import Data.Profunctor.Product (ProductProfunctor, SumProfunctor)
import Data.Profunctor.Product.Default (Default)

data MonomorphicProduct = Product Int Bool            deriving Generic
data MonomorphicSum     = A Int | B Bool              deriving Generic
data MonomorphicBoth    = Both1 Char | Both2 Int Bool deriving Generic

instance _ => Default p MonomorphicProduct MonomorphicProduct

instance _ => Default p MonomorphicSum MonomorphicSum

instance _ => Default p MonomorphicBoth MonomorphicBoth
