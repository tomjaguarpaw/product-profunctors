{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Profunctor.Product.Internal.TH where

import Data.Profunctor (dimap, lmap)
import Data.Profunctor.Product hiding (constructor, field)
import Data.Profunctor.Product.Default (Default, def)
import qualified Data.Profunctor.Product.Newtype as N
import Language.Haskell.TH (Dec(DataD, SigD, FunD, InstanceD, NewtypeD),
                            mkName, newName, nameBase, TyVarBndr(PlainTV, KindedTV),
                            Con(RecC, NormalC),
                            Clause(Clause),
                            Type(VarT, ForallT, AppT, ConT),
                            Body(NormalB), Q, classP,
                            Exp(ConE, VarE, AppE, TupE, LamE),
                            Pat(TupP, VarP, ConP), Name,
                            Info(TyConI), reify, conE, varE, varP)
import Control.Monad ((<=<))
import Control.Applicative (pure, liftA2, (<$>), (<*>))
import Control.Arrow (second)

makeAdaptorAndInstanceI :: Maybe String -> Name -> Q [Dec]
makeAdaptorAndInstanceI adaptorNameM = returnOrFail <=< r makeAandIE <=< reify
  where r = (return .)
        returnOrFail (Right decs) = decs
        returnOrFail (Left errMsg) = fail errMsg
        makeAandIE = makeAdaptorAndInstanceE adaptorNameM

type Error = String

makeAdaptorAndInstanceE :: Maybe String -> Info -> Either Error (Q [Dec])
makeAdaptorAndInstanceE adaptorNameM info = do
  dataDecStuff <- dataDecStuffOfInfo info
  let tyName  = dTyName  dataDecStuff
      tyVars  = dTyVars  dataDecStuff
      conName = dConName dataDecStuff
      conTys  = dConTys  dataDecStuff

      numTyVars = length tyVars
      numConTys = lengthCons conTys
      defaultAdaptorName = (mkName . ("p" ++) . nameBase) conName
      adaptorNameN = maybe defaultAdaptorName mkName adaptorNameM
      adaptorSig' = adaptorSig tyName numTyVars adaptorNameN
      adaptorDefinition' =
        case conTys of ConTys   _        ->
                         adaptorDefinition numTyVars conName adaptorNameN
                       FieldTys fieldTys ->
                         adaptorDefinitionFields conName fieldTys adaptorNameN

      instanceDefinition' = instanceDefinition tyName numTyVars numConTys
                                               adaptorNameN conName

      newtypeInstance' = if numConTys == 1 then
                           newtypeInstance conName tyName
                         else
                           return []

  return $ do
    as <- sequence [adaptorSig', adaptorDefinition', instanceDefinition']
    ns <- newtypeInstance'
    return (as ++ ns)

newtypeInstance :: Name -> Name -> Q [Dec]
newtypeInstance conName tyName = do
  x <- newName "x"

  let body = [ FunD 'N.constructor [simpleClause (NormalB (ConE conName))]
             , FunD 'N.field [simpleClause (NormalB (LamE [ConP conName [VarP x]] (VarE x)))] ]
#if __GLASGOW_HASKELL__ >= 800
  return [InstanceD Nothing [] (ConT ''N.Newtype `AppT` ConT tyName) body]
#else
  return [InstanceD [] (ConT ''N.Newtype `AppT` ConT tyName) body]
#endif

data ConTysFields = ConTys   [Type]
                  -- ^^ The type of each constructor field
                  | FieldTys [(Name, Type)]
                  -- ^^ The fieldname and type of each constructor field

lengthCons :: ConTysFields -> Int
lengthCons (ConTys l)   = length l
lengthCons (FieldTys l) = length l

data DataDecStuff = DataDecStuff {
    dTyName  :: Name
  , dTyVars  :: [Name]
  , dConName :: Name
  , dConTys  :: ConTysFields
  }

dataDecStuffOfInfo :: Info -> Either Error DataDecStuff
#if __GLASGOW_HASKELL__ >= 800
dataDecStuffOfInfo (TyConI (DataD _cxt tyName tyVars _kind constructors _deriving)) =
#else
dataDecStuffOfInfo (TyConI (DataD _cxt tyName tyVars constructors _deriving)) =
#endif
  do
    (conName, conTys) <- extractConstructorStuff constructors
    let tyVars' = map varNameOfBinder tyVars
    return DataDecStuff { dTyName  = tyName
                        , dTyVars  = tyVars'
                        , dConName = conName
                        , dConTys  = conTys
                        }

#if __GLASGOW_HASKELL__ >= 800
dataDecStuffOfInfo (TyConI (NewtypeD _cxt tyName tyVars _kind constructor _deriving)) =
#else
dataDecStuffOfInfo (TyConI (NewtypeD _cxt tyName tyVars constructor _deriving)) =
#endif
  do
    (conName, conTys) <- extractConstructorStuff [constructor]
    let tyVars' = map varNameOfBinder tyVars
    return DataDecStuff { dTyName  = tyName
                        , dTyVars  = tyVars'
                        , dConName = conName
                        , dConTys  = conTys
                        }
dataDecStuffOfInfo _ = Left "That doesn't look like a data or newtype declaration to me"

varNameOfBinder :: TyVarBndr -> Name
varNameOfBinder (PlainTV n) = n
varNameOfBinder (KindedTV n _) = n

conStuffOfConstructor :: Con -> Either Error (Name, ConTysFields)
conStuffOfConstructor (NormalC conName st) = do
  return (conName, ConTys (map snd st))
conStuffOfConstructor (RecC conName vst) = do
  return (conName, FieldTys (map (\(n, _, t) -> (n, t)) vst))

conStuffOfConstructor _ = Left "I can't deal with your constructor type"

constructorOfConstructors :: [Con] -> Either Error Con
constructorOfConstructors [single] = return single
constructorOfConstructors [] = Left "I need at least one constructor"
constructorOfConstructors _many =
  Left "I can't deal with more than one constructor"

extractConstructorStuff :: [Con] -> Either Error (Name, ConTysFields)
extractConstructorStuff = conStuffOfConstructor <=< constructorOfConstructors

instanceDefinition :: Name -> Int -> Int -> Name -> Name -> Q Dec
instanceDefinition tyName' numTyVars numConVars adaptorName' conName=instanceDec
  where instanceDec = liftA2
#if __GLASGOW_HASKELL__ >= 800
            (\i j -> InstanceD Nothing i j [defDefinition])
#else
            (\i j -> InstanceD i j [defDefinition])
#endif
            instanceCxt instanceType
        p = varTS "p"

        instanceCxt = mapM (uncurry classP) (pClass:defClasses)
        pClass :: Monad m => (Name, [m Type])
        pClass = (''ProductProfunctor, [return p])

        defaultPredOfVar :: String -> (Name, [Type])
        defaultPredOfVar fn = (''Default, [p,
                                           mkTySuffix "0" fn,
                                           mkTySuffix "1" fn])

        defClasses = map (second (map return) . defaultPredOfVar)
                         (allTyVars numTyVars)

        pArg :: String -> Type
        pArg s = pArg' tyName' s numTyVars

        instanceType = [t| $(pure $ ConT ''Default)
                           $(pure $ p)
                           $(pure $ pArg "0")
                           $(pure $ pArg "1")
                         |]

        defDefinition = FunD 'def [simpleClause defBody]
        defBody = NormalB(VarE adaptorName' `AppE` appEAll (ConE conName) defsN)
        defsN = replicate numConVars (VarE 'def)

adaptorSig :: Name -> Int -> Name -> Q Dec
adaptorSig tyName' numTyVars n = fmap (SigD n) adaptorType
  where p = mkName "p"
        adaptorType = ForallT scope <$> adaptorCxt <*> adaptorAfterCxt
        adaptorAfterCxt = [t| $before -> $after |]
        adaptorCxt = fmap (:[]) (classP ''ProductProfunctor [pType])
        before = foldl (liftA2 AppT) (pure (ConT tyName')) pArgs
        pType = pure $ VarT p
        pArgs = map pApp tyVars
        pApp :: String  -> Q Type
        pApp v = [t| $pType $(mkVarTsuffix "0" v) $(mkVarTsuffix "1" v) |]


        tyVars = allTyVars numTyVars

        pArg :: String -> Q Type
        pArg s = pure $ pArg' tyName' s numTyVars

        after = [t| $pType $(pArg "0") $(pArg "1") |]

        scope = concat [ [PlainTV p]
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
                            25 -> 'p25
                            26 -> 'p26
                            27 -> 'p27
                            28 -> 'p28
                            29 -> 'p29
                            30 -> 'p30
                            31 -> 'p31
                            32 -> 'p32
                            33 -> 'p33
                            34 -> 'p34
                            35 -> 'p35
                            36 -> 'p36
                            37 -> 'p37
                            38 -> 'p38
                            39 -> 'p39
                            40 -> 'p40
                            41 -> 'p41
                            42 -> 'p42
                            43 -> 'p43
                            44 -> 'p44
                            45 -> 'p45
                            46 -> 'p46
                            47 -> 'p47
                            48 -> 'p48
                            49 -> 'p49
                            50 -> 'p50
                            51 -> 'p51
                            52 -> 'p52
                            53 -> 'p53
                            54 -> 'p54
                            55 -> 'p55
                            56 -> 'p56
                            57 -> 'p57
                            58 -> 'p58
                            59 -> 'p59
                            60 -> 'p60
                            61 -> 'p61
                            62 -> 'p62
                            _  -> error errorMsg
  where errorMsg = "Data.Profunctor.Product.TH: "
                   ++ show n
                   ++ " is too many type variables for me!"

adaptorDefinition :: Int -> Name -> Name -> Q Dec
adaptorDefinition numConVars conName x = fmap (FunD x . pure) clause
  where clause = fmap (\b -> Clause [] b wheres) body
        toTupleN = mkName "toTuple"
        fromTupleN = mkName "fromTuple"
        toTupleE = varE toTupleN
        fromTupleE = varE fromTupleN
        theDimap = [| $(varE 'dimap) $toTupleE $fromTupleE |]
        pN = varE (tupleAdaptors numConVars)
        body = fmap NormalB [| $theDimap . $pN . $toTupleE |]
        wheres = [toTuple conName (toTupleN, numConVars),
                  fromTuple conName (fromTupleN, numConVars)]

adaptorDefinitionFields :: Name -> [(Name, name)] -> Name -> Q Dec
adaptorDefinitionFields conName fieldsTys adaptorName =
  fmap (FunD adaptorName . pure) clause
  where fields = map fst fieldsTys
        -- TODO: vv f should be generated in Q
        fP = varP (mkName "f")
        fE = varE (mkName "f")
        clause = liftA2 (\fP' b -> Clause [fP'] (NormalB b) []) fP body
        body = case fields of
          []             -> error "Can't handle no fields in constructor"
          field1:fields' ->
            let first =
                  [| $(varE '(***$)) $(conE conName) $(theLmap field1) |]
                app x y =
                  [| $(varE '(****)) $x $(theLmap y) |]
            in foldl app first fields'

        theLmap field =
          [| $(varE 'lmap) $(varE field) ($(varE field) $fE) |]

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

varS :: String -> Exp
varS = VarE . mkName

varPS :: String -> Pat
varPS = VarP . mkName

mkTyVarsuffix :: String -> String -> TyVarBndr
mkTyVarsuffix s = PlainTV . mkName . (++s)

mkTySuffix :: String -> String -> Type
mkTySuffix s = varTS . (++s)

mkVarTsuffix :: String -> String -> Q Type
mkVarTsuffix s = pure . VarT . mkName . (++s)

varTS :: String -> Type
varTS = VarT . mkName

appTAll :: Type -> [Type] -> Type
appTAll = foldl AppT

appEAll :: Exp -> [Exp] -> Exp
appEAll = foldl AppE

simpleClause :: Body -> Clause
simpleClause x = Clause [] x []
