{-# LANGUAGE TemplateHaskell #-}
module Data.Profunctor.Product.Tuples.TH
  ( mkTs
  , pTns
  , mkFlattenNs
  , mkUnflattenNs
  , pNs
  , mkDefaultNs
  , maxTupleSize
  ) where

import Language.Haskell.TH

import Data.Profunctor (Profunctor (dimap))
import Data.Profunctor.Product.Class (ProductProfunctor, (***!), empty)
import Data.Profunctor.Product.Default.Class (Default (def))
import Control.Applicative (pure)

mkTs :: [Int] -> Q [Dec]
mkTs = mapM mkT

mkT :: Int -> Q Dec
mkT n = tySynD (tyName n) tyVars tyDef
  where
    tyName n' = mkName ('T':show n')
    tyVars = map PlainTV . take n $ allNames
    tyDef = case n of
      0 -> tupleT 0
      1 -> varT (head allNames)
      _ -> tupleT 2 `appT` varT (head allNames) `appT` applyT (n - 1)
    applyT n' = foldl (\t v -> t `appT` varT v) (conT (tyName n')) (take n' (tail allNames))
    allNames = [ mkName $ c:show i | i <- [0::Int ..], c <- ['a'..'z'] ]

chain :: ProductProfunctor p => (t -> p a2 b2) -> (p a1 b1, t)
      -> p (a1, a2) (b1, b2)
chain rest (a, as) = uncurry (***!) (a, rest as)

pTns :: [Int] -> Q [Dec]
pTns = fmap concat . mapM pTn

productProfunctor :: Name -> Q Pred
productProfunctor p = classP ''ProductProfunctor [pure (VarT p)]

default_ :: Name -> Name -> Name -> Q Pred
default_ p a b = classP ''Default (map (pure . VarT) [p, a, b])

pTn :: Int -> Q [Dec]
pTn n = sequence [sig, fun]
  where
    p = mkName "p"
    sig = sigD (pT n) (forallT (map PlainTV $ p : take n as ++ take n bs)
                               (sequence [productProfunctor p])
                               (arrowT `appT` mkLeftTy `appT` mkRightTy)
                      )
    mkLeftTy = foldl appT (conT tN)
             $ zipWith (\a b -> varT p `appT` varT a `appT` varT b) (take n as) (take n bs)
    mkRightTy = varT p `appT` foldl appT (conT tN) (map varT . take n $ as)
                       `appT` foldl appT (conT tN) (map varT . take n $ bs)
    fun = funD (pT n) [ clause [] (normalB bdy) [] ]
    bdy = case n of
      0 -> [| const empty |]
      1 -> [| id |]
      2 -> [| uncurry (***!) |]
      _ -> varE 'chain `appE` varE (pT (n - 1))
    pT n' = mkName ("pT" ++ show n')
    tN = mkName ('T':show n)
    as = [ mkName $ 'a':show i | i <- [0::Int ..] ]
    bs = [ mkName $ 'b':show i | i <- [0::Int ..] ]

mkFlattenNs :: [Int] -> Q [Dec]
mkFlattenNs = fmap concat . mapM mkFlattenN

mkFlattenN :: Int -> Q [Dec]
mkFlattenN n = sequence [sig, fun]
  where
    sig = sigD nm (forallT (map PlainTV names) (pure []) $ arrowT `appT` unflatT names `appT` flatT names)
    fun = funD nm [ clause [mkTupPat names] (normalB bdy) [] ]
    bdy = mkFlatExp names
    unflatT [] = tupleT 0
    unflatT [v] = varT v
    unflatT (v:vs) = tupleT 2 `appT` varT v `appT` unflatT vs
    flatT [] = tupleT 0
    flatT [v] = varT v
    flatT vs = foldl appT (tupleT (length vs)) (map varT vs)
    mkTupPat [] = tupP []
    mkTupPat [v] = varP v
    mkTupPat (v:vs) = tupP [varP v, mkTupPat vs]
    mkFlatExp [] = tupE []
    mkFlatExp [v] = varE v
    mkFlatExp vs = tupE (map varE vs)
    nm = mkName ("flatten" ++ show n)
    names = take n [ mkName $ c:show i | i <- [0::Int ..], c <- ['a'..'z'] ]

mkUnflattenNs :: [Int] -> Q [Dec]
mkUnflattenNs = fmap concat . mapM mkUnflattenN

mkUnflattenN :: Int -> Q [Dec]
mkUnflattenN n = sequence [sig, fun]
  where
    sig = sigD nm (forallT (map PlainTV names) (pure []) $ arrowT `appT` flatT names `appT` unflatT names)
    fun = funD nm [ clause [mkTupPat names] (normalB bdy) [] ]
    bdy = mkUnflatExp names
    unflatT [] = tupleT 0
    unflatT [v] = varT v
    unflatT (v:vs) = tupleT 2 `appT` varT v `appT` unflatT vs
    flatT [] = tupleT 0
    flatT [v] = varT v
    flatT vs = foldl appT (tupleT (length vs)) (map varT vs)
    mkTupPat [] = tupP []
    mkTupPat [v] = varP v
    mkTupPat vs = tupP (map varP vs)
    mkUnflatExp [] = tupE []
    mkUnflatExp [v] = varE v
    mkUnflatExp (v:vs) = tupE [varE v, mkUnflatExp vs]
    nm = mkName ("unflatten" ++ show n)
    names = take n [ mkName $ c:show i | i <- [0::Int ..], c <- ['a'..'z'] ]

pNs :: [Int] -> Q [Dec]
pNs = fmap concat . mapM pN

pN :: Int -> Q [Dec]
pN n = sequence [sig, fun]
  where
    sig = sigD nm (forallT (map PlainTV $ p : as ++ bs)
                           (sequence [productProfunctor p])
                           (arrowT `appT` mkLeftTy `appT` mkRightTy)
                   )
    mkLeftTy = case n of
      1 -> mkPT (head as) (head bs)
      _ -> foldl appT (tupleT n) (zipWith mkPT as bs)
    mkRightTy = varT p `appT` mkTupT as `appT` mkTupT bs
    mkTupT = foldl appT (tupleT n) . map varT
    mkPT a b = varT p `appT` varT a `appT` varT b
    fun = funD nm [ clause [] (normalB bdy) [] ]
    bdy = varE 'convert `appE` unflat `appE` unflat `appE` flat `appE` pT
    unflat = varE $ mkName unflatNm
    flat = varE $ mkName flatNm
    pT = varE $ mkName pTNm
    unflatNm = "unflatten" ++ show n
    flatNm = "flatten" ++ show n
    pTNm = "pT" ++ show n
    nm = mkName ('p':show n)
    p = mkName "p"
    as = take n [ mkName $ 'a':show i | i <- [0::Int ..] ]
    bs = take n [ mkName $ 'b':show i | i <- [0::Int ..] ]

convert :: Profunctor p => (a2 -> a1) -> (tp -> tTp) -> (b1 -> b2)
                           -> (tTp -> p a1 b1)
                           -> tp -> p a2 b2
convert u u' f c = dimap u f . c . u'

mkDefaultNs :: [Int] -> Q [Dec]
mkDefaultNs = mapM mkDefaultN

mkDefaultN :: Int -> Q Dec
mkDefaultN n = instanceD (sequence (productProfunctor p : mkDefs))
                         (conT ''Default `appT` varT p `appT` mkTupT as `appT` mkTupT bs)
                         [mkFun]
  where
    mkDefs = zipWith (\a b -> default_ p a b) as bs
    mkTupT = foldl appT (tupleT n) . map varT
    mkFun = funD 'def [clause [] bdy []]
    bdy = normalB $ case n of
      0 -> varE 'empty
      _ -> varE (mkName $ 'p':show n) `appE` tupE (replicate n (varE 'def))
    p = mkName "p"
    as = take n [ mkName $ 'a':show i | i <- [0::Int ..] ]
    bs = take n [ mkName $ 'b':show i | i <- [0::Int ..] ]

maxTupleSize :: Int
maxTupleSize = 35
