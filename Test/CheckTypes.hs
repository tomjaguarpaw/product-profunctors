module CheckTypes where

import Data.Profunctor.Product (ProductProfunctor)
import Data.Profunctor.Product.Default (Default, def)

import Definitions (Data2, Data3, Record2, Record3,
                    RecordDefaultName,
                    pData2, pData3, pRecord2, pRecord3,
                    pRecordDefaultName)

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

defaultNameGenerated :: ProductProfunctor p => RecordDefaultName (p x x') (p y y') -> p (RecordDefaultName x y) (RecordDefaultName x' y')
defaultNameGenerated = pRecordDefaultName
