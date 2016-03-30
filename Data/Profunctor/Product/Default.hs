{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             FlexibleContexts #-}

module Data.Profunctor.Product.Default where

import Control.Applicative (Const (Const))
import Data.Functor.Identity (Identity (Identity))
import Data.Profunctor (Profunctor (dimap))
-- TODO: vv this imports a lot of names.  Should we list them all?
import Data.Profunctor.Product

class Default p a b where
  -- Would rather call it "default", but that's a keyword
  def :: p a b

cdef :: Default (PPOfContravariant u) a a => u a
cdef = unPPOfContravariant def

instance ProductProfunctor p => Default p () () where
  def = empty

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2)
         => Default p (a1, a2) (b1, b2) where
  def = p2 (def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3)
         => Default p (a1, a2, a3)
                      (b1, b2, b3) where
  def = p3 (def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4)
         => Default p (a1, a2, a3, a4)
                      (b1, b2, b3, b4) where
  def = p4 (def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5)
         => Default p (a1, a2, a3, a4, a5)
                      (b1, b2, b3, b4, b5) where
  def = p5 (def, def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6)
         => Default p (a1, a2, a3, a4, a5, a6)
                      (b1, b2, b3, b4, b5, b6) where
  def = p6 (def, def, def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7)
         => Default p (a1, a2, a3, a4, a5, a6, a7)
                      (b1, b2, b3, b4, b5, b6, b7) where
  def = p7 (def, def, def, def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8)
                      (b1, b2, b3, b4, b5, b6, b7, b8) where
  def = p8 (def, def, def, def, def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8, a9)
                      (b1, b2, b3, b4, b5, b6, b7, b8, b9) where
  def = p9 (def, def, def, def, def, def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9, Default p a10 b10)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8,
                       a9, a10)
                      (b1, b2, b3, b4, b5, b6, b7, b8,
                      b9, b10) where
  def = p10 (def, def, def, def, def, def, def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9, Default p a10 b10, Default p a11 b11)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8,
                       a9, a10, a11)
                      (b1, b2, b3, b4, b5, b6, b7, b8,
                      b9, b10, b11) where
  def = p11 (def, def, def, def, def, def, def, def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9, Default p a10 b10, Default p a11 b11,
          Default p a12 b12)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8,
                       a9, a10, a11, a12)
                      (b1, b2, b3, b4, b5, b6, b7, b8,
                      b9, b10, b11, b12) where
  def = p12 (def, def, def, def, def, def, def, def, def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9, Default p a10 b10, Default p a11 b11,
          Default p a12 b12, Default p a13 b13)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8,
                       a9, a10, a11, a12, a13)
                      (b1, b2, b3, b4, b5, b6, b7, b8,
                      b9, b10, b11, b12, b13) where
  def = p13 (def, def, def, def, def, def, def, def, def, def,
             def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9, Default p a10 b10, Default p a11 b11,
          Default p a12 b12, Default p a13 b13, Default p a14 b14)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8,
                       a9, a10, a11, a12, a13, a14)
                      (b1, b2, b3, b4, b5, b6, b7, b8,
                      b9, b10, b11, b12, b13, b14) where
  def = p14 (def, def, def, def, def, def, def, def, def, def,
             def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9, Default p a10 b10, Default p a11 b11,
          Default p a12 b12, Default p a13 b13, Default p a14 b14, Default p a15 b15)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8,
                       a9, a10, a11, a12, a13, a14, a15)
                      (b1, b2, b3, b4, b5, b6, b7, b8,
                      b9, b10, b11, b12, b13, b14, b15) where
  def = p15 (def, def, def, def, def, def, def, def, def, def,
             def, def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9, Default p a10 b10, Default p a11 b11,
          Default p a12 b12, Default p a13 b13, Default p a14 b14,
          Default p a15 b15, Default p a16 b16)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8,
                       a9, a10, a11, a12, a13, a14, a15, a16)
                      (b1, b2, b3, b4, b5, b6, b7, b8,
                      b9, b10, b11, b12, b13, b14, b15, b16) where
  def = p16 (def, def, def, def, def, def, def, def, def, def,
             def, def, def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9, Default p a10 b10, Default p a11 b11,
          Default p a12 b12, Default p a13 b13, Default p a14 b14,
          Default p a15 b15, Default p a16 b16, Default p a17 b17)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8,
                       a9, a10, a11, a12, a13, a14, a15, a16, a17)
                      (b1, b2, b3, b4, b5, b6, b7, b8,
                      b9, b10, b11, b12, b13, b14, b15, b16, b17) where
  def = p17 (def, def, def, def, def, def, def, def, def, def,
             def, def, def, def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9, Default p a10 b10, Default p a11 b11,
          Default p a12 b12, Default p a13 b13, Default p a14 b14,
          Default p a15 b15, Default p a16 b16, Default p a17 b17,
          Default p a18 b18)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8,
                       a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
                      (b1, b2, b3, b4, b5, b6, b7, b8,
                      b9, b10, b11, b12, b13, b14, b15, b16, b17, b18) where
  def = p18 (def, def, def, def, def, def, def, def, def, def,
             def, def, def, def, def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9, Default p a10 b10, Default p a11 b11,
          Default p a12 b12, Default p a13 b13, Default p a14 b14,
          Default p a15 b15, Default p a16 b16, Default p a17 b17,
          Default p a18 b18, Default p a19 b19)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8,
                       a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
                      (b1, b2, b3, b4, b5, b6, b7, b8,
                      b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19) where
  def = p19 (def, def, def, def, def, def, def, def, def, def,
             def, def, def, def, def, def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9, Default p a10 b10, Default p a11 b11,
          Default p a12 b12, Default p a13 b13, Default p a14 b14,
          Default p a15 b15, Default p a16 b16, Default p a17 b17,
          Default p a18 b18, Default p a19 b19, Default p a20 b20)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8,
                       a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
                      (b1, b2, b3, b4, b5, b6, b7, b8,
                      b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20) where
  def = p20 (def, def, def, def, def, def, def, def, def, def,
             def, def, def, def, def, def, def, def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9, Default p a10 b10, Default p a11 b11,
          Default p a12 b12, Default p a13 b13, Default p a14 b14,
          Default p a15 b15, Default p a16 b16, Default p a17 b17,
          Default p a18 b18, Default p a19 b19, Default p a20 b20,
          Default p a21 b21)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8,
                       a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)
                      (b1, b2, b3, b4, b5, b6, b7, b8,
                      b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21) where
  def = p21 (def, def, def, def, def, def, def, def, def, def,
             def, def, def, def, def, def, def, def, def, def,
             def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9, Default p a10 b10, Default p a11 b11,
          Default p a12 b12, Default p a13 b13, Default p a14 b14,
          Default p a15 b15, Default p a16 b16, Default p a17 b17,
          Default p a18 b18, Default p a19 b19, Default p a20 b20,
          Default p a21 b21, Default p a22 b22)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8,
                       a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22)
                      (b1, b2, b3, b4, b5, b6, b7, b8,
                      b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22) where
  def = p22 (def, def, def, def, def, def, def, def, def, def,
             def, def, def, def, def, def, def, def, def, def,
             def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9, Default p a10 b10, Default p a11 b11,
          Default p a12 b12, Default p a13 b13, Default p a14 b14,
          Default p a15 b15, Default p a16 b16, Default p a17 b17,
          Default p a18 b18, Default p a19 b19, Default p a20 b20,
          Default p a21 b21, Default p a22 b22, Default p a23 b23)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8,
                       a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23)
                      (b1, b2, b3, b4, b5, b6, b7, b8,
                      b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22, b23) where
  def = p23 (def, def, def, def, def, def, def, def, def, def,
             def, def, def, def, def, def, def, def, def, def,
             def, def, def)

instance (ProductProfunctor p, Default p a1 b1, Default p a2 b2,
          Default p a3 b3, Default p a4 b4, Default p a5 b5,
          Default p a6 b6, Default p a7 b7, Default p a8 b8,
          Default p a9 b9, Default p a10 b10, Default p a11 b11,
          Default p a12 b12, Default p a13 b13, Default p a14 b14,
          Default p a15 b15, Default p a16 b16, Default p a17 b17,
          Default p a18 b18, Default p a19 b19, Default p a20 b20,
          Default p a21 b21, Default p a22 b22, Default p a23 b23,
          Default p a24 b24)
         => Default p (a1, a2, a3, a4, a5, a6, a7, a8,
                       a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24)
                      (b1, b2, b3, b4, b5, b6, b7, b8,
                      b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22, b23, b24) where
  def = p24 (def, def, def, def, def, def, def, def, def, def,
             def, def, def, def, def, def, def, def, def, def,
             def, def, def, def)

instance (Profunctor p, Default p a b) => Default p (Identity a) (Identity b)
  where
    def = dimap (\(Identity a) -> a) Identity def

instance (Profunctor p, Default p a b) => Default p (Const a c) (Const b c)
  where
    def = dimap (\(Const a) -> a) Const def
