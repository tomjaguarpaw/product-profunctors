-- Compare genericAdaptor with hand-written, specialized adaptors.
-- They should optimize to identical Core.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

import Criterion.Main
import Control.DeepSeq
import GHC.Generics

import Data.Profunctor.Product
import Data.Profunctor.Product.Adaptor

deriving instance Generic (a, b, c, d, e, f, g, h, i, j)
instance NFData (Int, Bool, Int, Bool, Int, Int, Bool, Int, Bool, Int)

p3_G :: ProductProfunctor p => Adaptor p (p a a', p b b', p c c')
p3_G = genericAdaptor

p3_0 (fa, fb, fc) (a, b, c) = (fa a, fb b, fc c)

t3 :: (Int -> Int, Bool -> Bool, Double -> Double)
t3 = (id, id, id)

u3 :: (Int, Bool, Double)
u3 = (0, False, 0)

p10_G
  :: ProductProfunctor p
  => Adaptor p
    ( p a a', p b b', p c c', p d d', p e e'
    , p f f', p g g', p h h', p i i', p j j')
p10_G = genericAdaptor

p10_0 (fa, fb, fc, fd, fe, ff, fg, fh, fi, fj) (a, b, c, d, e, f, g, h, i, j) =
  (fa a, fb b, fc c, fd d, fe e, ff f, fg g, fh h, fi i, fj j)

t10
  :: (Int -> Int, Bool -> Bool, Int -> Int, Bool -> Bool, Int -> Int
  ,   Int -> Int, Bool -> Bool, Int -> Int, Bool -> Bool, Int -> Int)
t10 = (id, id, id, id, id, id, id, id, id, id)

u10 :: (Int, Bool, Int, Bool, Int, Int, Bool, Int, Bool, Int)
u10 = (0, False, 1, True, 2, 3, False, 4, True, 5)

main = defaultMain
  [ bench "p3_0" $ nf (p3_0 t3) u3
  , bench "p3_G" $ nf (\u -> u `seq` p3_G t3 u) u3

  , bench "p3_0-bis" $ nf (uncurry p3_0) (t3, u3)
  , bench "p3_G-ter" $ nf (\(t, u) -> u `seq` p3_G t u) (t3, u3)

  , bench "p10_0" $ nf (p10_0 t10) u10
  , bench "p10_G" $ nf (\u -> u `seq` p10_G t10 u) u10

  , bench "p10_0-bis" $ nf (uncurry p10_0) (t10, u10)
  , bench "p10_G-ter" $ nf (\(t, u) -> u `seq` p10_G t u) (t10, u10)
  ]
