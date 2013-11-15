module Data.Profunctor.Product.TH where

import Language.Haskell.TH (Dec(DataD, SigD, FunD, InstanceD),
                            mkName, TyVarBndr(PlainTV),
                            Con(RecC), Strict(NotStrict), Clause(Clause),
                            Type(VarT, ForallT, AppT, ArrowT, ConT),
                            Body(NormalB), Q, Pred(ClassP),
                            Exp(ConE, VarE, InfixE, AppE, TupE),
                            Pat(TupP, VarP, ConP), Name)

data MakeRecordT = MakeRecordT { typeName :: String
                               , constructorName :: String
                               , fieldNames :: [String]
                               , deriving_ :: [String]
                               , adaptorName :: String }

makeRecord :: MakeRecordT -> Q [Dec]
makeRecord r = return decs
  where MakeRecordT tyName conName tyVars derivings _ = r
        decs = [datatype, pullerSig', pullerDefinition', instanceDefinition]
        datatype = DataD [] tyName' tyVars' [con] derivings'
          where fields = map toField tyVars
                tyVars' = map (PlainTV . mkName) tyVars
                con = RecC (mkName conName) fields
                toField s = (mkName s, NotStrict, VarT (mkName s))
                derivings' = map mkName derivings
        tyName' = mkName tyName

        pArg :: String -> Type
        pArg = appTAll (ConT tyName') . conArgs
               where conArgs :: String -> [Type]
                     conArgs s = map (mkVarTsuffix s) tyVars

        pullerName = mkName (adaptorName r)

        pullerSig' = pullerSig pullerName tyName' tyVars pArg

        pullerDefinition' = pullerDefinition pullerName tyVars conName

        instanceDefinition = InstanceD instanceCxt instanceType [defDefinition]
          where instanceCxt = map (uncurry ClassP) (pClass:defClasses)
                pClass = (mkName "ProductProfunctor", [varTS "p"])

                defaultPredOfVar :: String -> (Name, [Type])
                defaultPredOfVar fn = (mkName "Default", [varTS "p",
                                                          mkTySuffix "0" fn,
                                                          mkTySuffix "1" fn])

                defClasses = map defaultPredOfVar tyVars

                instanceType = appTAll (conTS "Default")
                                       [varTS "p", pArg "0", pArg "1"]

                defDefinition = FunD (mkName "def") [Clause [] defBody []]
                defBody = NormalB (VarE pullerName
                                   `AppE` appEAll
                                          (conES conName) defsN)
                defsN = map (const (varS "def")) tyVars

pullerSig :: Name -> Name -> [String] -> ([Char] -> Type) -> Dec
pullerSig pullerName tyName' tyVars pArg = SigD pullerName pullerType
  where pullerType = ForallT scope pullerCxt pullerAfterCxt
        pullerAfterCxt = before `appArrow` after
        pullerCxt = [ClassP (mkName "ProductProfunctor")
                            [VarT (mkName "p")]]
        before = appTAll (ConT tyName') pArgs
        pType = VarT (mkName "p")
        pArgs = map (\v -> (appTAll pType
                                    [mkVarTsuffix "0" v, mkVarTsuffix "1" v]))
                    tyVars

        after = appTAll pType [pArg "0", pArg "1"]

        scope = concat [ [PlainTV (mkName "p")]
                       , map (mkTyVarsuffix "0") tyVars
                       , map (mkTyVarsuffix "1") tyVars ]

pullerDefinition :: Name -> [String] -> String -> Dec
pullerDefinition pullerName tyVars conName = FunD pullerName [clause]
  where clause = Clause [] body wheres
        toTupleN = mkName "toTuple"
        fromTupleN = mkName "fromTuple"
        toTupleE = VarE toTupleN
        fromTupleE = VarE fromTupleN
        theDimap = appEAll (varS "dimap") [toTupleE, fromTupleE]
        pN = VarE (mkName ("p" ++ show (length tyVars)))
        body = NormalB (theDimap `o` pN `o` toTupleE)
        wheres = [toTuple conName (toTupleN, tyVars),
                  fromTuple conName (fromTupleN, tyVars)]

xTuple :: ([Pat] -> Pat) -> ([Exp] -> Exp) -> (Name, [String]) -> Dec
xTuple patCon retCon (funN, tyVars) = FunD funN [clause]
  where clause = Clause [pat] body []
        pat = patCon varPats
        body = NormalB (retCon varExps)
        varPats = map varPS tyVars
        varExps = map varS tyVars

fromTuple :: String -> (Name, [String]) -> Dec
fromTuple conName = xTuple patCon retCon
  where patCon = TupP
        retCon = appEAll (conES conName)

toTuple :: String -> (Name, [String]) -> Dec
toTuple conName = xTuple patCon retCon
  where patCon = conPS conName
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
