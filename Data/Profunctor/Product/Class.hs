module Data.Profunctor.Product.Class where

import Prelude hiding (id)
import Control.Arrow (Arrow, (***))
import Control.Category (id)
import Data.Profunctor (Profunctor, WrappedArrow)

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
--
-- It's easy to make instances of 'ProductProfunctor'.  Just make
-- instances
--
-- @
--  instance Profunctor MyProductProfunctor where
--    ...
--
--  instance Applicative (MyProductProfunctor a) where
--    ...
-- @
--
-- and then write
--
-- @
--  instance ProductProfunctor Writer where
--    empty  = defaultEmpty
--    (***!) = defaultProfunctorProduct
-- @
class Profunctor p => ProductProfunctor p where
  empty  :: p () ()
  (***!) :: p a b -> p a' b' -> p (a, a') (b, b')

instance ProductProfunctor (->) where
  empty  = id
  (***!) = (***)

instance Arrow arr => ProductProfunctor (WrappedArrow arr) where
  empty  = id
  (***!) = (***)

class Profunctor p => SumProfunctor p where
  -- Morally we should have 'zero :: p Void Void' but I don't think
  -- that would actually be useful
  (+++!) :: p a b -> p a' b' -> p (Either a a') (Either b b')

instance SumProfunctor (->) where
  f +++! g = either (Left . f) (Right . g)
