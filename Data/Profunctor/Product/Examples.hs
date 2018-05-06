{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Profunctor.Product.Examples where

import qualified Data.Profunctor                 as P
import qualified Data.Profunctor.Product         as PP
import qualified Data.Profunctor.Product.Default as D
import           Control.Applicative             (Applicative, liftA2, pure, (<*>))

newtype Replicator r f a b = Replicator (r -> f b)
  deriving Functor

instance Applicative f => D.Default (Replicator (f b) f) b b where
  def = Replicator id

-- | A higher-order generalisation of 'Prelude.replicate'.  For
-- example
--
-- @
-- foo :: IO (String, String, String)
-- foo = replicateT getLine
-- @
--
-- @
-- > foo
-- Hello
-- world
-- !
-- (\"Hello\",\"world\",\"!\")
-- @
replicateT :: D.Default (Replicator r f) b b => r -> f b
replicateT = f
  where Replicator f = def'
        def' :: D.Default p a a => p a a
        def' = D.def

-- Boilerplate that is derivable using generics but I never got round
-- to implementing it.
instance Applicative f => Applicative (Replicator r f a) where
  pure = Replicator . pure . pure
  Replicator f <*> Replicator x = Replicator (liftA2 (<*>) f x)

instance Functor f => P.Profunctor (Replicator r f) where
  dimap g h (Replicator f) = Replicator ((fmap . fmap) h f)

instance Applicative f=> PP.ProductProfunctor (Replicator r f) where
  purePP = pure
  (****) = (<*>)
