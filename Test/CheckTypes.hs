{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module CheckTypes where

import Data.Profunctor.Product (ProductProfunctor)
import Data.Profunctor.Product.Default (Default, def)
import Data.Profunctor.Product.Adaptor

import Definitions (Data2, Data3, Record2, Record3,
                    RecordDefaultName,
                    Data2Inferrable(Data2Inferrable),
                    Record2Inferrable(Record2Inferrable),
                    pData2, pData3, pRecord2, pRecord3,
                    pRecordDefaultName,
                    unArrow, Unit(Unit), point)
import DefinitionsUndecidable ()

-- The test suite checks that the TH derived adaptor is of the correct
-- type and that the typeclass instance has been generated.  We don't
-- actually check the implementation since by parametricity I expect
-- that only the order in which the product profunctors are combined
-- can vary.

pData2' :: ProductProfunctor p =>
           Data2 (p a a') (p b b') -> p (Data2 a b) (Data2 a' b')
pData2' = pData2

pData3' :: ProductProfunctor p =>
           Data3 (p a a') (p b b') (p c c') -> p (Data3 a b c) (Data3 a' b' c')
pData3' = pData3

pRecord2' :: ProductProfunctor p =>
           Record2 (p a a') (p b b') -> p (Record2 a b) (Record2 a' b')
pRecord2' = pRecord2

pRecord3' :: ProductProfunctor p =>
           Record3 (p a a') (p b b') (p c c') -> p (Record3 a b c) (Record3 a' b' c')
pRecord3' = pRecord3

instanceData2 :: (ProductProfunctor p, Default p a a', Default p b b')
                 => p (Data2 a b) (Data2 a' b')
instanceData2 = def

instanceData3 :: (ProductProfunctor p,
                  Default p a a', Default p b b', Default p c c')
                 => p (Data3 a b c) (Data3 a' b' c')
instanceData3 = def

instanceRecord2 :: (ProductProfunctor p, Default p a a', Default p b b')
                 => p (Record2 a b) (Record2 a' b')
instanceRecord2 = def

instanceRecord3 :: (ProductProfunctor p,
                  Default p a a', Default p b b', Default p c c')
                 => p (Record3 a b c) (Record3 a' b' c')
instanceRecord3 = def

defaultNameGenerated :: ProductProfunctor p => RecordDefaultName (p x x') (p y y')
                     -> p (RecordDefaultName x y) (RecordDefaultName x' y')
defaultNameGenerated = pRecordDefaultName

-- We similarly test the type of the generic adaptor.

pData2G :: ProductProfunctor p =>
           Data2 (p a a') (p b b') -> p (Data2 a b) (Data2 a' b')
pData2G = genericAdaptor

pData3G :: ProductProfunctor p =>
           Data3 (p a a') (p b b') (p c c') -> p (Data3 a b c) (Data3 a' b' c')
pData3G = genericAdaptor

pRecord2G :: ProductProfunctor p
          => Record2 (p a a') (p b b') -> p (Record2 a b) (Record2 a' b')
pRecord2G = pRecord2

pRecord3G :: ProductProfunctor p
          => Record3 (p a a') (p b b') (p c c') -> p (Record3 a b c) (Record3 a' b' c')
pRecord3G = pRecord3

-- Can type inference information flow from the left type argument of
-- a Profunctor to the right?
inferDataLR :: ()
inferDataLR   = const () (unArrow def (Data2Inferrable   Unit Unit))

inferRecordLR :: ()
inferRecordLR = const () (unArrow def (Record2Inferrable Unit Unit))

-- Can type inference information flow from the right type argument of
-- a Profunctor to the left?
inferDataRL :: ()
inferDataRL   = case unArrow def point of Data2Inferrable Unit Unit -> ()

inferRecordRL :: ()
inferRecordRL = case unArrow def point of Record2Inferrable Unit Unit -> ()

data a :~: b where
  Refl :: a :~: a

pData2TypeEq
  :: (Data2 (p a a') (p b b') -> p (Data2 a b) (Data2 a' b'))
  :~: Adaptor p (Data2 (p a a') (p b b'))
pData2TypeEq = Refl

pData3TypeEq
  :: (Data3 (p a a') (p b b') (p c c') -> p (Data3 a b c) (Data3 a' b' c'))
  :~: Adaptor p (Data3 (p a a') (p b b') (p c c'))
pData3TypeEq = Refl
