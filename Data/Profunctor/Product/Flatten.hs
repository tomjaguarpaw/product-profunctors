{-# LANGUAGE TemplateHaskell #-}

module Data.Profunctor.Product.Flatten where

import Data.Profunctor.Product.Tuples.TH (mkFlattenNs, mkUnflattenNs, maxTupleSize)

mkFlattenNs [0..maxTupleSize]
mkUnflattenNs [0..maxTupleSize]
