module Data.Profunctor.Product.TH where

import Language.Haskell.TH (Dec(DataD, SigD), mkName, TyVarBndr(PlainTV),
                            Con(RecC), Strict(NotStrict),
                            Type(VarT, ForallT, AppT, ArrowT, ConT),
                            Q, Pred(ClassP))

makeRecord :: String -> String -> [String] -> Q [Dec]
makeRecord tyName conName tyVars = return [datatype, pullerSig]
  where datatype = DataD [] tyName' tyVars' [con] derivings
          where fields = map toField tyVars
                tyVars' = map (PlainTV . mkName) tyVars
                con = RecC (mkName conName) fields
                toField s = (mkName s, NotStrict, VarT (mkName s))
                derivings = map mkName ["Generic", "Eq", "Show"]
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

                scope = concat [ [PlainTV (mkName "p")]
                               , map (PlainTV . mkName . (++"0")) tyVars
                               , map (PlainTV . mkName . (++"1")) tyVars ]
