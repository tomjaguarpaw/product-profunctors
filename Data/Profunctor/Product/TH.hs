{-# LANGUAGE TemplateHaskell #-}

-- | If you have a data declaration which is a polymorphic product,
-- for example
--
-- @
-- data Foo a b c = Foo a b c
-- @
--
-- or
--
-- @
-- data Foo a b c = Foo { foo :: a, bar :: b, baz :: c }
-- @
--
-- then you can use Template Haskell to automatically derive the
-- product-profunctor 'Default' instances and product-profunctor
-- \"adaptor\" with the following import and splice:
--
-- @
-- $(makeAdaptorAndInstance \"pFoo\" ''Foo)
-- @
--
-- * The adaptor for a type Foo is by convention called pFoo, but in
-- practice you can call it anything.
--
-- The instance generated will be
--
-- @
-- instance (ProductProfunctor p, Default p a a', Default p b b', Default p c c')
--       => Default p (Foo a b c) (Foo a' b' c')
-- @
--
-- and pFoo will have the type
--
-- @
-- pFoo :: ProductProfunctor p =>
--         Foo (p a a') (p b b') (p c c') -> p (Foo a b c) (Foo a' b' c')
-- @

module Data.Profunctor.Product.TH where

import Data.Profunctor (dimap)
import Data.Profunctor.Product (ProductProfunctor, p1, p2, p3, p4, p5, p6, p7,
                                p8, p9, p10, p11, p12, p13, p14, p15, p16, p17,
                                p18, p19, p20, p21, p22, p23, p24)
import Data.Profunctor.Product.Default (Default, def)
import Data.Profunctor.Product.Newtype (Newtype(constructor, field))
import Language.Haskell.TH (Dec(DataD, SigD, FunD, InstanceD, NewtypeD),
                            mkName, newName, TyVarBndr(PlainTV, KindedTV),
                            Con(RecC, NormalC),
                            Strict(NotStrict), Clause(Clause),
                            Type(VarT, ForallT, AppT, ArrowT, ConT),
                            Body(NormalB), Q, classP,
                            Exp(ConE, VarE, InfixE, AppE, TupE, LamE),
                            Pat(TupP, VarP, ConP), Name,
                            Info(TyConI), reify)
import Control.Monad ((<=<))
import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow (second)

makeAdaptorAndInstance :: String -> Name -> Q [Dec]
makeAdaptorAndInstance adaptorNameS = returnOrFail <=< r makeAandIE <=< reify
  where r = (return .)
        returnOrFail (Right decs) = decs
        returnOrFail (Left errMsg) = fail errMsg
        makeAandIE = makeAdaptorAndInstanceE adaptorNameS

type Error = String

makeAdaptorAndInstanceE :: String -> Info -> Either Error (Q [Dec])
makeAdaptorAndInstanceE adaptorNameS info = do
  (tyName, tyVars, conName, conTys) <- dataDecStuffOfInfo info
  let numTyVars = length tyVars
      numConTys = length conTys
      adaptorNameN = mkName adaptorNameS
      adaptorSig' = adaptorSig tyName numTyVars adaptorNameN
      adaptorDefinition' = adaptorDefinition numTyVars conName adaptorNameN
      instanceDefinition' = instanceDefinition tyName numTyVars numConTys
                                               adaptorNameN conName

      newtypeInstance :: Q [Dec]
      newtypeInstance = case conTys of
        [_] -> do
          x <- newName "x"

          let body = [ FunD 'constructor [Clause [] (NormalB (ConE conName)) [] ]
                     , FunD 'field [Clause [] (NormalB (LamE [ConP conName [VarP x]] (VarE x))) []] ]

          return [InstanceD [] (ConT ''Newtype `AppT` ConT tyName) body]
        _     -> return []

  return $ do
    as <- sequence [adaptorSig', pure adaptorDefinition', instanceDefinition']
    ns <- newtypeInstance
    return (as ++ ns)


dataDecStuffOfInfo :: Info -> Either Error (Name, [Name], Name, [Name])
dataDecStuffOfInfo (TyConI (DataD _cxt tyName tyVars constructors _deriving)) =
  do
    (conName, conTys) <- extractConstructorStuff constructors
    let tyVars' = map varNameOfBinder tyVars
    return (tyName, tyVars', conName, conTys)
dataDecStuffOfInfo (TyConI (NewtypeD _cxt tyName tyVars constructor _deriving)) =
  do
    (conName, conTys) <- extractConstructorStuff [constructor]
    let tyVars' = map varNameOfBinder tyVars
    return (tyName, tyVars', conName, conTys)
dataDecStuffOfInfo _ = Left "That doesn't look like a data or newtpe declaration to me"

varNameOfType :: Type -> Either Error Name
varNameOfType (VarT n) = Right n
varNameOfType x = Left $ "Found a non-variable type" ++ show x

varNameOfBinder :: TyVarBndr -> Name
varNameOfBinder (PlainTV n) = n
varNameOfBinder (KindedTV n _) = n

conStuffOfConstructor :: Con -> Either Error (Name, [Name])
conStuffOfConstructor (NormalC conName st) = do
  conTys <- mapM (varNameOfType . snd) st
  return (conName, conTys)
conStuffOfConstructor (RecC conName vst) = do
  conTys <- mapM (varNameOfType . thrd) vst
  return (conName, conTys)
    where thrd = \(_,_,x) -> x
conStuffOfConstructor _ = Left "I can't deal with your constructor type"

constructorOfConstructors :: [Con] -> Either Error Con
constructorOfConstructors [single] = return single
constructorOfConstructors [] = Left "I need at least one constructor"
constructorOfConstructors _many = Left msg
  where msg = "I can't deal with more than one constructor"

extractConstructorStuff :: [Con] -> Either Error (Name, [Name])
extractConstructorStuff = conStuffOfConstructor <=< constructorOfConstructors

-- MakeRecordT and makeRecordData were from an old interface.  We could probably
-- delete them now.
data MakeRecordT = MakeRecordT { typeName :: String
                               , constructorName :: String
                               , fieldNames :: [String]
                               , deriving_ :: [String]
                               , adaptorName :: String }

makeRecordData :: MakeRecordT -> Q [Dec]
makeRecordData r = return [datatype'] where
  MakeRecordT tyName conName tyVars derivings _ = r
  tyName' = mkName tyName
  datatype' = datatype tyName' tyVars conName derivings

makeRecord :: MakeRecordT -> Q [Dec]
makeRecord r = decs
  where MakeRecordT tyName conName tyVars derivings _ = r
        decs = (\a i -> [datatype', a, adaptorDefinition', i])
               <$> adaptorSig'
               <*> instanceDefinition'
        tyName' = mkName tyName
        conName' = mkName conName

        adaptorName' = mkName (adaptorName r)

        numTyVars = length tyVars

        datatype' = datatype tyName' tyVars conName derivings
        adaptorSig' = adaptorSig tyName' numTyVars adaptorName'
        adaptorDefinition' = adaptorDefinition numTyVars conName' adaptorName'
        instanceDefinition' = instanceDefinition tyName' numTyVars numTyVars
                                                 adaptorName' conName'

-- The implementations of the datatype (only used in the old makeRecord),
-- instance and adaptor follow.
datatype :: Name -> [String] -> String -> [String] -> Dec
datatype tyName tyVars conName derivings = datatype'
  where datatype' = DataD [] tyName tyVars' [con] derivings'
        fields = map toField tyVars
        tyVars' = map (PlainTV . mkName) tyVars
        con = RecC (mkName conName) fields
        toField s = (mkName s, NotStrict, VarT (mkName s))
        derivings' = map mkName derivings

instanceDefinition :: Name -> Int -> Int -> Name -> Name -> Q Dec
instanceDefinition tyName' numTyVars numConVars adaptorName' conName=instanceDec
  where instanceDec = fmap (\i -> InstanceD i instanceType [defDefinition])
                      instanceCxt
        instanceCxt = mapM (uncurry classP) (pClass:defClasses)
        pClass :: Monad m => (Name, [m Type])
        pClass = (''ProductProfunctor, [return (varTS "p")])

        defaultPredOfVar :: String -> (Name, [Type])
        defaultPredOfVar fn = (''Default, [varTS "p",
                                           mkTySuffix "0" fn,
                                           mkTySuffix "1" fn])

        defClasses = map (second (map return) . defaultPredOfVar)
                         (allTyVars numTyVars)

        pArg :: String -> Type
        pArg s = pArg' tyName' s numTyVars

        instanceType = appTAll (ConT ''Default)
                               [varTS "p", pArg "0", pArg "1"]

        defDefinition = FunD 'def [Clause [] defBody []]
        defBody = NormalB(VarE adaptorName' `AppE` appEAll (ConE conName) defsN)
        defsN = replicate numConVars (VarE 'def)

adaptorSig :: Name -> Int -> Name -> Q Dec
adaptorSig tyName' numTyVars n = fmap (SigD n) adaptorType
  where adaptorType = fmap (\a -> ForallT scope a adaptorAfterCxt) adaptorCxt
        adaptorAfterCxt = before `appArrow` after
        adaptorCxt = fmap (:[]) (classP ''ProductProfunctor [return (VarT (mkName "p"))])
        before = appTAll (ConT tyName') pArgs
        pType = VarT (mkName "p")
        pArgs = map pApp tyVars
        pApp :: String  -> Type
        pApp v = appTAll pType [mkVarTsuffix "0" v, mkVarTsuffix "1" v]


        tyVars = allTyVars numTyVars

        pArg :: String -> Type
        pArg s = pArg' tyName' s numTyVars

        after = appTAll pType [pArg "0", pArg "1"]

        scope = concat [ [PlainTV (mkName "p")]
                       , map (mkTyVarsuffix "0") tyVars
                       , map (mkTyVarsuffix "1") tyVars ]

-- This should probably fail in a more graceful way than an error. I
-- guess via Either or Q.
tupleAdaptors :: Int -> Name
tupleAdaptors n = case n of 1  -> 'p1
                            2  -> 'p2
                            3  -> 'p3
                            4  -> 'p4
                            5  -> 'p5
                            6  -> 'p6
                            7  -> 'p7
                            8  -> 'p8
                            9  -> 'p9
                            10 -> 'p10
                            11 -> 'p11
                            12 -> 'p12
                            13 -> 'p13
                            14 -> 'p14
                            15 -> 'p15
                            16 -> 'p16
                            17 -> 'p17
                            18 -> 'p18
                            19 -> 'p19
                            20 -> 'p20
                            21 -> 'p21
                            22 -> 'p22
                            23 -> 'p23
                            24 -> 'p24
                            _  -> error errorMsg
  where errorMsg = "Data.Profunctor.Product.TH: "
                   ++ show n
                   ++ " is too many type variables for me!"

adaptorDefinition :: Int -> Name -> Name -> Dec
adaptorDefinition numConVars conName = flip FunD [clause]
  where clause = Clause [] body wheres
        toTupleN = mkName "toTuple"
        fromTupleN = mkName "fromTuple"
        toTupleE = VarE toTupleN
        fromTupleE = VarE fromTupleN
        theDimap = appEAll (VarE 'dimap) [toTupleE, fromTupleE]
        pN = VarE (tupleAdaptors numConVars)
        body = NormalB (theDimap `o` pN `o` toTupleE)
        wheres = [toTuple conName (toTupleN, numConVars),
                  fromTuple conName (fromTupleN, numConVars)]

xTuple :: ([Pat] -> Pat) -> ([Exp] -> Exp) -> (Name, Int) -> Dec
xTuple patCon retCon (funN, numTyVars) = FunD funN [clause]
  where clause = Clause [pat] body []
        pat = patCon varPats
        body = NormalB (retCon varExps)
        varPats = map varPS (allTyVars numTyVars)
        varExps = map varS (allTyVars numTyVars)

fromTuple :: Name -> (Name, Int) -> Dec
fromTuple conName = xTuple patCon retCon
  where patCon = TupP
        retCon = appEAll (ConE conName)

toTuple :: Name -> (Name, Int) -> Dec
toTuple conName = xTuple patCon retCon
  where patCon = ConP conName
        retCon = TupE

{-
Note that we can also do the instance definition like this, but it would
require pulling the to/fromTuples to the top level

instance (ProductProfunctor p, Default p a a', Default p b b',
          Default p c c', Default p d d', Default p e e',
          Default p f f', Default p g g', Default p h h')
         => Default p (LedgerRow' a b c d e f g h)
                      (LedgerRow' a' b' c' d' e' f' g' h') where
  def = dimap tupleOfLedgerRow lRowOfTuple def
-}

pArg' :: Name -> String -> Int -> Type
pArg' tn s = appTAll (ConT tn) . map (varTS . (++s)) . allTyVars

allTyVars :: Int -> [String]
allTyVars numTyVars = map varA tyNums
  where varA i = "a" ++ show i ++ "_"
        tyNums :: [Int]
        tyNums = [1..numTyVars]

o :: Exp -> Exp -> Exp
o x y = InfixE (Just x) (varS ".") (Just y)

varS :: String -> Exp
varS = VarE . mkName

varPS :: String -> Pat
varPS = VarP . mkName

mkTyVarsuffix :: String -> String -> TyVarBndr
mkTyVarsuffix s = PlainTV . mkName . (++s)

mkTySuffix :: String -> String -> Type
mkTySuffix s = varTS . (++s)

mkVarTsuffix :: String -> String -> Type
mkVarTsuffix s = VarT . mkName . (++s)

varTS :: String -> Type
varTS = VarT . mkName

appTAll :: Type -> [Type] -> Type
appTAll = foldl AppT

appEAll :: Exp -> [Exp] -> Exp
appEAll = foldl AppE

appArrow :: Type -> Type -> Type
appArrow l r = appTAll ArrowT [l, r]
