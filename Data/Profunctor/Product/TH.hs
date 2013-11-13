module Data.Profunctor.Product.TH where

import Language.Haskell.TH (Dec(DataD), mkName, TyVarBndr(PlainTV), Con(RecC),
                            Strict(NotStrict), Type(VarT), Q)

makeRecord :: String -> String -> [String] -> Q [Dec]
makeRecord tyName conName tyVars = return [datatype]
  where datatype = DataD [] tyName' tyVars' [con] []
          where fields = map (\s -> (mkName s, NotStrict, VarT (mkName s))) tyVars
                tyName' = mkName tyName
                tyVars' = map (PlainTV . mkName) tyVars
                con = RecC (mkName conName) fields