module Data.Profunctor.Product.Class where

import Data.Profunctor (Profunctor)

-- | A 'ProductProfunctor' is a generalization of an 'Applicative'.
-- It has an "input", contravariant type parameter on the left as well
-- as the usual 'Applicative' "output", covariant parameter on the
-- right.
--
-- You will find it easier to see the similarity between
-- 'ProductProfunctor' and 'Applicative' if you look at @purePP@,
-- @***$@, and @****@, which correspond to @pure@, @<$>@, and @<*>@
-- respectively.
class Profunctor p => ProductProfunctor p where
  empty :: p () ()
  (***!) :: p a b -> p a' b' -> p (a, a') (b, b')

