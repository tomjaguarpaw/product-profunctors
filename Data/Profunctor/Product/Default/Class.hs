{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Profunctor.Product.Default.Class where

class Default p a b where
  -- Would rather call it "default", but that's a keyword
  def :: p a b
