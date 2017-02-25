{-# LANGUAGE TemplateHaskell #-}

-- | This is old cruft.  You should never use this and it will likely
-- be deprecated in a future version.

module Data.Profunctor.Product.Tuples where

import Data.Profunctor.Product.Tuples.TH

mkTs [0..maxTupleSize]
