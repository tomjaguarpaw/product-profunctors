module Data.Profunctor.Product.Class where

import Data.Profunctor (Profunctor)

-- | 'ProductProfunctor' is a generalization of 'Applicative'.
--
-- It has the usual 'Applicative' "output" (covariant) parameter on
-- the right.  Additionally it has an "input" (contravariant) type
-- parameter on the left.
--
-- You will find it easier to see the similarity between
-- 'ProductProfunctor' and 'Applicative' if you look at @purePP@,
-- @***$@, and @****@, which correspond to @pure@, @<$>@, and @<*>@
-- respectively.
class Profunctor p => ProductProfunctor p where
  empty :: p () ()
  (***!) :: p a b -> p a' b' -> p (a, a') (b, b')

