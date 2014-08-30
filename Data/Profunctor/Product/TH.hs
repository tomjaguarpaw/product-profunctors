module Data.Profunctor.Product.TH where

import Language.Haskell.TH (Dec(DataD, SigD, FunD, InstanceD),
                            mkName, TyVarBndr(PlainTV, KindedTV),
                            Con(RecC, NormalC),
                            Strict(NotStrict), Clause(Clause),
                            Type(VarT, ForallT, AppT, ArrowT, ConT),
                            Body(NormalB), Q, Pred(ClassP),
                            Exp(ConE, VarE, InfixE, AppE, TupE),
                            Pat(TupP, VarP, ConP), Name,
                            Info(TyConI), reify)
import Control.Monad ((<=<))

-- Usage note: Use this library by running the following splice:
--
-- $(makeAdaptorAndInstance "pTypeName" ''TypeName)
--
-- where 'TypeName' is the name of your type and 'pTypeName' is the name you
-- want for your adaptor.

-- The template Haskell requires you import a number of other names.
-- I'm not sure if there's a good way to make this neater.
--
-- import Data.Profunctor.Product (ProductProfunctor, p<n>)
-- ^^ where <n> is the number of fields in your record
-- import Data.Profunctor (dimap)
-- import Data.Profunctor.Product.Default (Default, def)

-- TODO: ^^ lens avoids forcing the user to explicitly import the
-- stuff it needs to generate lenses.  We should copy that.

makeAdaptorAndInstance :: String -> Name -> Q [Dec]
makeAdaptorAndInstance adaptorNameS = returnOrFail <=< r makeAandIE <=< reify
  where r = (return .)
        returnOrFail (Right decs) = return decs
        returnOrFail (Left errMsg) = fail errMsg
        makeAandIE = makeAdaptorAndInstanceE adaptorNameS

type Error = String

makeAdaptorAndInstanceE :: String -> Info -> Either Error [Dec]
makeAdaptorAndInstanceE adaptorNameS info = do
  (tyName, tyVars, conName, conTys) <- dataDecStuffOfInfo info
  let numTyVars = length tyVars
      numConTys = length conTys
      adaptorNameN = mkName adaptorNameS
      adaptorSig' = adaptorSig tyName numTyVars adaptorNameN
      adaptorDefinition' = adaptorDefinition numTyVars conName adaptorNameN
      instanceDefinition' = instanceDefinition tyName numTyVars numConTys
                                               adaptorNameN conName

  return [adaptorSig', adaptorDefinition', instanceDefinition']

-- TODO: support newtypes?
dataDecStuffOfInfo :: Info -> Either Error (Name, [Name], Name, [Name])
dataDecStuffOfInfo (TyConI (DataD _cxt tyName tyVars constructors _deriving)) =
  do
    (conName, conTys) <- extractConstructorStuff constructors
    let tyVars' = map varNameOfBinder tyVars
    return (tyName, tyVars', conName, conTys)
dataDecStuffOfInfo _ = Left "That doesn't look like a data declaration to me"

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
makeRecord r = return decs
  where MakeRecordT tyName conName tyVars derivings _ = r
        decs = [datatype', adaptorSig', adaptorDefinition', instanceDefinition']
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

instanceDefinition :: Name -> Int -> Int -> Name -> Name -> Dec
instanceDefinition tyName' numTyVars numConVars adaptorName' conName=instanceDec
  where instanceDec = InstanceD instanceCxt instanceType [defDefinition]
        instanceCxt = map (uncurry ClassP) (pClass:defClasses)
        pClass = (mkName "ProductProfunctor", [varTS "p"])

        defaultPredOfVar :: String -> (Name, [Type])
        defaultPredOfVar fn = (mkName "Default", [varTS "p",
                                                  mkTySuffix "0" fn,
                                                  mkTySuffix "1" fn])

        defClasses = map defaultPredOfVar (allTyVars numTyVars)

        pArg :: String -> Type
        pArg s = pArg' tyName' s numTyVars

        instanceType = appTAll (conTS "Default")
                               [varTS "p", pArg "0", pArg "1"]

        defDefinition = FunD (mkName "def") [Clause [] defBody []]
        defBody = NormalB(VarE adaptorName' `AppE` appEAll (ConE conName) defsN)
        defsN = replicate numConVars (varS "def")

adaptorSig :: Name -> Int -> Name -> Dec
adaptorSig tyName' numTyVars = flip SigD adaptorType
  where adaptorType = ForallT scope adaptorCxt adaptorAfterCxt
        adaptorAfterCxt = before `appArrow` after
        adaptorCxt = [ClassP (mkName "ProductProfunctor")
                            [VarT (mkName "p")]]
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

adaptorDefinition :: Int -> Name -> Name -> Dec
adaptorDefinition numConVars conName = flip FunD [clause]
  where clause = Clause [] body wheres
        toTupleN = mkName "toTuple"
        fromTupleN = mkName "fromTuple"
        toTupleE = VarE toTupleN
        fromTupleE = VarE fromTupleN
        theDimap = appEAll (varS "dimap") [toTupleE, fromTupleE]
        pN = VarE (mkName ("p" ++ show numConVars))
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

conES :: String -> Exp
conES = ConE . mkName

conPS :: String -> [Pat] -> Pat
conPS = ConP . mkName

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

conTS :: String -> Type
conTS = ConT . mkName

appTAll :: Type -> [Type] -> Type
appTAll = foldl AppT

appEAll :: Exp -> [Exp] -> Exp
appEAll = foldl AppE

appArrow :: Type -> Type -> Type
appArrow l r = appTAll ArrowT [l, r]
