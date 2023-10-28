module Data.Profunctor.Product.Newtype where

import qualified Data.Profunctor as P

class Newtype t where
  constructor :: a -> t a
  field       :: t a -> a

pNewtype :: (P.Profunctor p, Newtype t) => p a b -> p (t a) (t b)
pNewtype = P.dimap field constructor

-- when you have a newtype that wraps a newtype
-- templateTemplate = pNewtype2 $ requiredTableField "id"
pNewtype2 :: (Profunctor p, Newtype t1, Newtype t2) => p a b -> p (t1 (t2 a))(t1 (t2 b))
pNewtype2 = dimap (field . field) (constructor . constructor)

-- tableId :: (Newtype t) => (TableFields (Maybe (t (Field SqlUuid))) (t (Field SqlUuid)))
-- tableId = fpNewtype $ optionalTableField "id"
fpNewtype :: (Functor f, Profunctor p, Newtype t) => p (f a) b -> p (f (t a)) (t b)
fpNewtype = dimap (field <$>) constructor

mapNewtype :: (Newtype t) => (a -> b) -> t a -> t b
mapNewtype f = constructor . f . field

-- when you have a newtype that wraps a newtype
mapNewtype2 :: (Newtype t1, Newtype t2) => (a -> b) -> t1 (t2 a) -> t1 (t2 b)
mapNewtype2 = mapNewtype . mapNewtype

-- removeNothing :: (Newtype t) => t [Maybe a] -> [t a]
-- removeNothing = traverseT catMaybes
traverseT :: (Functor f, Newtype t1, Newtype t2) => (a -> f b) -> t1 a -> f (t2 b)
traverseT f ns = constructor <$> f (field ns)

traverseT_ :: (Functor f, Newtype t1, Newtype t2) => (a -> f b) -> t1 a -> f (t2 ())
traverseT_ f ns = constructor () <$ f (field ns)

sequenceT :: (Functor f, Newtype t) => t (f a) -> f (t a)
sequenceT = traverseT id

sequenceT_ :: (Functor f, Newtype t) => t (f a) -> f (t ())
sequenceT_ = traverseT_ id

forT :: (Functor f, Newtype t1, Newtype t2) => t1 a -> (a -> f b) -> f (t2 b)
forT = flip traverseT

forT_ :: (Functor f, Newtype t1, Newtype t2) => t1 a -> (a -> f b) -> f (t2 ())
forT_ = flip traverseT_
