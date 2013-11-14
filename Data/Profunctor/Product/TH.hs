module Data.Profunctor.Product.TH where

import Language.Haskell.TH (Dec(DataD, SigD, FunD), mkName, TyVarBndr(PlainTV),
                            Con(RecC), Strict(NotStrict), Clause(Clause),
                            Type(VarT, ForallT, AppT, ArrowT, ConT),
                            Body(NormalB), Q, Pred(ClassP),
                            Exp(ConE, VarE, InfixE, AppE, TupE),
                            Pat(TupP, VarP, ConP))

makeRecord :: String -> String -> [String] -> [String] -> Q [Dec]
makeRecord tyName conName tyVars derivings = return decs
  where decs = [datatype, pullerSig, pullerDefinition]
        datatype = DataD [] tyName' tyVars' [con] derivings'
          where fields = map toField tyVars
                tyVars' = map (PlainTV . mkName) tyVars
                con = RecC (mkName conName) fields
                toField s = (mkName s, NotStrict, VarT (mkName s))
                derivings' = map mkName derivings
        tyName' = mkName tyName

        pullerSig = SigD pullerName pullerType
          where pullerName = mkName ("p" ++ conName)
                pullerType = ForallT scope pullerCxt pullerAfterCxt
                pullerAfterCxt = ArrowT `AppT` before `AppT` after
                pullerCxt = [ClassP (mkName "ProductProfunctor")
                                    [VarT (mkName "p")]]
                before = foldl AppT (ConT tyName') pArgs
                pType = VarT (mkName "p")
                pArgs = map (\v -> (pType
                                    `AppT` (mkVarTsuffix "0" v)
                                    `AppT` (mkVarTsuffix "1" v))) tyVars

                after = pType `AppT` (pArg "0") `AppT` (pArg "1")

                pArg :: String -> Type
                pArg = foldl AppT (ConT tyName') . conArgs

                conArgs :: String -> [Type]
                conArgs s = map (mkVarTsuffix s) tyVars

                mkVarTsuffix :: String -> String -> Type
                mkVarTsuffix s = VarT . mkName . (++s)

                mkTyVarsuffix :: String -> String -> TyVarBndr
                mkTyVarsuffix s = PlainTV . mkName . (++s)

                scope = concat [ [PlainTV (mkName "p")]
                               , map (mkTyVarsuffix "0") tyVars
                               , map (mkTyVarsuffix "1") tyVars ]

        pullerDefinition = FunD (mkName ("p" ++ conName)) [clause]
          where clause = Clause [] body wheres
                toTuple = varS "toTuple"
                theDimap = varS "dimap"
                           `AppE` toTuple
                           `AppE` varS "fromTuple"
                pN = VarE (mkName ("p" ++ show (length tyVars)))
                body = NormalB (theDimap `o` pN `o` toTuple)
                o x y = InfixE (Just x) (varS ".") (Just y)
                wheres = [whereToTuple, whereFromTuple]
                -- FIXME: names the wrong way round!
                whereToTuple = FunD (mkName "fromTuple") [toTupleClause]
                  where toTupleClause = Clause [toTuplePat] toTupleBody []
                        toTuplePat = TupP (map (VarP . mkName) tyVars)
                        cone = ConE (mkName conName)
                        toTupleBody =NormalB (foldl AppE cone (map varS tyVars))
                whereFromTuple = FunD (mkName "toTuple") [fromTupleClause]
                  where fromTupleClause = Clause [fromTuplePat] fromTupleBody []
                        fromTuplePat = (conp (map (VarP . mkName) tyVars))
                        conp = ConP (mkName conName)
                        fromTupleBody = NormalB (TupE (map varS tyVars))
                varS :: String -> Exp
                varS = VarE . mkName
