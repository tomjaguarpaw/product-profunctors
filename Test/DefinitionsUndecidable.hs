{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | We define the data types and generate the TH in a separate module
-- because we want to ensure that no external names are required to be
-- imported.
--
-- It's a bit sad that these need UndecidableInstances

module DefinitionsUndecidable where

import GHC.Generics (Generic)
import Data.Profunctor.Product (ProductProfunctor, SumProfunctor)
import Data.Profunctor.Product.Default
  (Default, DefaultFields', DefaultConstraints, DefaultConstraints')

data MonomorphicProduct = Product Int Bool            deriving Generic
data MonomorphicSum     = A Int | B Bool              deriving Generic
data MonomorphicBoth    = Both1 Char | Both2 Int Bool deriving Generic
data PolyProduct a b c  = PolyProduct a b c           deriving Generic

instance (ProductProfunctor p, DefaultFields' p MonomorphicProduct)
         => Default p MonomorphicProduct MonomorphicProduct

instance (SumProfunctor p, DefaultFields' p MonomorphicSum)
         => Default p MonomorphicSum MonomorphicSum

instance (DefaultConstraints' p MonomorphicBoth)
         => Default p MonomorphicBoth MonomorphicBoth

instance
  ( DefaultConstraints p
      (PolyProduct a b c)
      (PolyProduct a' b' c')
  ) => Default p (PolyProduct a b c) (PolyProduct a' b' c')

-- A constraint @c@.
data Dict c where
  Dict :: c => Dict c

-- Entailment @c => d@.
data c :- d where
  Sub :: (c => Dict d) -> c :- d

-- Equivalence of constraints.
type c :<=>: d = (c :- d, d :- c)

checkDFMonomorphicProduct
  :: DefaultFields' p MonomorphicProduct :<=>:
     (Default p Int Int, Default p Bool Bool)
checkDFMonomorphicProduct = (Sub Dict, Sub Dict)

checkDFMonomorphicSum
  :: DefaultFields' p MonomorphicSum :<=>:
     (Default p Int Int, Default p Bool Bool)
checkDFMonomorphicSum = (Sub Dict, Sub Dict)

checkDCMonomorphicBoth
  :: DefaultConstraints' p MonomorphicBoth :<=>:
     (ProductProfunctor p, SumProfunctor p, Default p Int Int, Default p Bool Bool, Default p Char Char)
checkDCMonomorphicBoth = (Sub Dict, Sub Dict)

checkDCPolyProduct
  :: DefaultConstraints p (PolyProduct a b c) (PolyProduct a' b' c') :<=>:
     (ProductProfunctor p, Default p a a', Default p b b', Default p c c')
checkDCPolyProduct = (Sub Dict, Sub Dict)
