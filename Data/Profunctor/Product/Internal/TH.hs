{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Profunctor.Product.Internal.TH where

import Data.Profunctor (dimap, lmap)
import Data.Profunctor.Product hiding (constructor, field)
import Data.Profunctor.Product.Default (Default, def)
import qualified Data.Profunctor.Product.Newtype as N
import Language.Haskell.TH (Dec(DataD, SigD, FunD, InstanceD, NewtypeD),
                            mkName, newName, nameBase,
                            Con(RecC, NormalC),
                            Clause(Clause),
                            Type(VarT, ForallT, AppT, ConT),
                            Body(NormalB), Q,
                            Exp(ConE, VarE, AppE, TupE),
                            Pat(TupP, VarP, ConP), Name,
                            Info(TyConI), reify, conE, conT, varE, varP,
                            instanceD, Overlap(Incoherent), Pred)
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndr_, TyVarBndrSpec,
                                               plainTVSpecified, tvName)
import Control.Monad ((<=<))
import Control.Applicative (liftA2)

makeAdaptorAndInstanceI :: Bool -> Maybe String -> Name -> Q [Dec]
makeAdaptorAndInstanceI inferrable adaptorNameM =
  returnOrFail <=< r makeAandIE <=< reify
  where r = (return .)
        returnOrFail (Right decs) = decs
        returnOrFail (Left errMsg) = fail errMsg
        makeAandIE = makeAdaptorAndInstanceE sides adaptorNameM
        sides = case inferrable of
          True  -> [Just (Left ()), Just (Right ())]
          False -> [Nothing]

type Error = String

makeAdaptorAndInstanceE :: [Maybe (Either () ())]
                        -> Maybe String
                        -> Info
                        -> Either Error (Q [Dec])
makeAdaptorAndInstanceE sides adaptorNameM info = do
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
      adaptorDefinition' = case conTys of
        ConTys   _        -> adaptorDefinition numTyVars conName
        FieldTys fieldTys -> adaptorDefinitionFields conName fieldTys

      instanceDefinition' = map (\side ->
        instanceDefinition side tyName numTyVars numConTys adaptorNameN conName)
        sides

      newtypeInstance' = if numConTys == 1 then
                           newtypeInstance conName tyName
                         else
                           return []

  return $ do
    as <- sequence ( [ adaptorSig'
                     , adaptorDefinition' adaptorNameN ]
                   ++ instanceDefinition' )
    ns <- newtypeInstance'
    return (as ++ ns)

newtypeInstance :: Name -> Name -> Q [Dec]
newtypeInstance conName tyName = do
  x <- newName "x"

  let body = [d| $(varP 'N.constructor) = $(conE conName)
                 $(varP 'N.field) = \ $(pure $ conP conName [VarP x]) -> $(varE x) |]

  i <- do
    body' <- body
    instanceD (pure [])
                 [t| N.Newtype $(conT tyName) |]
                 (map pure body')
  pure [i]

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
dataDecStuffOfInfo (TyConI (DataD _cxt tyName tyVars _kind constructors _deriving)) =
  do
    (conName, conTys) <- extractConstructorStuff constructors
    let tyVars' = map varNameOfBinder tyVars
    return DataDecStuff { dTyName  = tyName
                        , dTyVars  = tyVars'
                        , dConName = conName
                        , dConTys  = conTys
                        }

dataDecStuffOfInfo (TyConI (NewtypeD _cxt tyName tyVars _kind constructor _deriving)) =
  do
    (conName, conTys) <- extractConstructorStuff [constructor]
    let tyVars' = map varNameOfBinder tyVars
    return DataDecStuff { dTyName  = tyName
                        , dTyVars  = tyVars'
                        , dConName = conName
                        , dConTys  = conTys
                        }
dataDecStuffOfInfo _ = Left "That doesn't look like a data or newtype declaration to me"

varNameOfBinder :: TyVarBndr_ flag -> Name
varNameOfBinder = tvName

conStuffOfConstructor :: Con -> Either Error (Name, ConTysFields)
conStuffOfConstructor = \case
  NormalC conName st -> return (conName, ConTys (map snd st))
  RecC conName vst -> return (conName, FieldTys (map (\(n, _, t) -> (n, t)) vst))
  _ -> Left "I can't deal with your constructor type"

constructorOfConstructors :: [Con] -> Either Error Con
constructorOfConstructors = \case
  [single] -> return single
  []       -> Left "I need at least one constructor"
  _many    -> Left "I can't deal with more than one constructor"

extractConstructorStuff :: [Con] -> Either Error (Name, ConTysFields)
extractConstructorStuff = conStuffOfConstructor <=< constructorOfConstructors

instanceDefinition :: Maybe (Either () ())
                   -> Name
                   -> Int
                   -> Int
                   -> Name
                   -> Name
                   -> Q Dec
instanceDefinition side tyName' numTyVars numConVars adaptorName' conName =
  instanceDec
  where instanceDec = do
          instanceCxt' <- instanceCxt
          instanceType' <- [t| Default $p $pArg0 $pArg1 |]
          defDefinition' <- [d| $(varP 'def) = $adaptorNameQ $(pure $ appEAll (ConE conName) defsN) |]
          pure (InstanceD (Incoherent <$ side) instanceCxt' instanceType' defDefinition')

        p :: Applicative m => m Type
        p = pure $ varTS "p"
        x = pure $ varTS "x"

        instanceCxt = do
            typeMatch' <- sequence typeMatch
            productProfunctor_p' <- [t| ProductProfunctor $p |]
            default_p_as0_as1 <- traverse default_p_a0_a1 (allTyVars numTyVars)
            pure (productProfunctor_p' : typeMatch' ++ default_p_as0_as1)

        (typeMatch, pArg0, pArg1) = case side of
            Nothing ->         ([],                       tyName0, tyName1)
            Just (Left ())  -> ([ [t| $x ~ $tyName0 |] ], x,       tyName1)
            Just (Right ()) -> ([ [t| $x ~ $tyName1 |] ], tyName0, x)

        tyName0 = tyName "0"
        tyName1 = tyName "1"

        default_p_a0_a1 :: String -> Q Pred
        default_p_a0_a1 a  = [t| Default $p $(tvar a "0") $(tvar a "1") |]

        tvar a i = pure (mkTySuffix i a)

        tyName :: String -> Q Type
        tyName suffix = pure $ pArg' tyName' suffix numTyVars

        adaptorNameQ = varE adaptorName'

        defsN = replicate numConVars (VarE 'def)

adaptorSig :: Name -> Int -> Name -> Q Dec
adaptorSig tyName' numTyVars n = fmap (SigD n) adaptorType
  where p = mkName "p"
        adaptorType = ForallT scope
                      <$> adaptorCxt
                      <*> [t| $before -> $pType $(pArg "0") $(pArg "1") |]
        adaptorCxt = fmap (:[]) [t| ProductProfunctor $pType |]
        before = foldl (\x y -> [t| $x $(pApp y) |]) (pure (ConT tyName')) tyVars
        pType = pure $ VarT p
        pApp :: String  -> Q Type
        pApp v = [t| $pType $(mkVarTsuffix "0" v) $(mkVarTsuffix "1" v) |]


        tyVars = allTyVars numTyVars

        pArg :: String -> Q Type
        pArg s = pure $ pArg' tyName' s numTyVars

        scope = concat [ [plainTVSpecified p]
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
adaptorDefinition numConVars conName x = do
  clause' <- clause
  pure ((FunD x . pure) clause')
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

tupP :: [Pat] -> Pat
tupP [p] = p
tupP ps  = TupP ps

tupE :: [Exp] -> Exp
tupE [e] = e
tupE es  = TupE
#if MIN_VERSION_template_haskell(2,16,0)
           $ map Just
#endif
           es

conP :: Name -> [Pat] -> Pat
conP conname = ConP conname
#if MIN_VERSION_template_haskell(2,18,0)
               []
#endif

fromTuple :: Name -> (Name, Int) -> Dec
fromTuple conName = xTuple patCon retCon
  where patCon = tupP
        retCon = appEAll (ConE conName)

toTuple :: Name -> (Name, Int) -> Dec
toTuple conName = xTuple patCon retCon
  where patCon = conP conName
        retCon = tupE

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

mkTyVarsuffix :: String -> String -> TyVarBndrSpec
mkTyVarsuffix s = plainTVSpecified . mkName . (++s)

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
