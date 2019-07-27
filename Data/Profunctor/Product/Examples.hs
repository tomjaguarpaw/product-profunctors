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
  dimap _ h (Replicator f) = Replicator ((fmap . fmap) h f)

instance Applicative f=> PP.ProductProfunctor (Replicator r f) where
  purePP = pure
  (****) = (<*>)

-- In the real world this would be 'StateT [a] Maybe b' but I don't want to
-- pick up the transformers dependency here
newtype Take a z b = Take ([a] -> Maybe ([a], b))
  deriving Functor

instance D.Default (Take a) z a where
  def = Take (\as ->
    case as of
      []      -> Nothing
      (a:as') -> Just (as', a))

-- | A type safe generalisation of 'Prelude.take'.  For example
--
-- @
-- > let count = [1..] :: [Int]
-- > takeT count :: Maybe (Int, Int)
-- Just (1,2)
-- > takeT count
--     :: Maybe (Int, Int, (Int, (Int, Int), Int, Int),
--               Const Int Bool, Identity (Int, Int), Tagged String Int)
-- Just (1,2,(3,(4,5),6,7),Const 8,Identity (9,10),Tagged 11)
-- @
takeT :: D.Default (Take a) b b
      => [a]
      -> Maybe b
takeT = takeExplicit D.def
  where takeExplicit :: Take a b b -> [a] -> Maybe b
        takeExplicit (Take f) as = fmap snd (f as)

-- More boilerplate
instance Applicative (Take a z) where
  pure x = Take (\as -> pure (as, x))
  Take f <*> Take x = Take (\as -> do
    (as', f')  <- f as
    (as'', x') <- x as'

    return (as'', f' x'))

instance P.Profunctor (Take a) where
  dimap _ g (Take h) = Take ((fmap . fmap . fmap) g h)

instance PP.ProductProfunctor (Take a) where
  purePP = pure
  (****) = (<*>)
