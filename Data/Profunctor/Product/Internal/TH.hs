{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Profunctor.Product.Internal.TH where

import Data.Profunctor (dimap, lmap)
import Data.Profunctor.Product hiding (constructor, field)
import Data.Profunctor.Product.Default (Default, def)
import qualified Data.Profunctor.Product.Newtype as N
import Language.Haskell.TH (Dec(DataD, SigD, InstanceD, NewtypeD),
                            mkName, newName, nameBase,
                            Con(RecC, NormalC),
                            Type(VarT, ForallT, AppT, ConT),
                            Q,
                            Exp(ConE, VarE, AppE, TupE, LamE),
                            Pat(TupP, VarP, ConP), Name,
                            Info(TyConI), reify, conE, conT, varE, varP,
                            instanceD, Overlap(Incoherent), Pred)
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndrSpec,
                                               plainTVSpecified, tvName)
import Control.Monad ((<=<))

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
    adaptorDefinition'' <- adaptorDefinition' adaptorNameN
    adaptorSig'' <- adaptorSig'
    instanceDefinition'' <- sequence instanceDefinition'
    ns <- newtypeInstance'
    return ([adaptorSig''] ++ adaptorDefinition'' ++ instanceDefinition'' ++ ns)

newtypeInstance :: Name -> Name -> Q [Dec]
newtypeInstance conName tyName = do
  let body = [d| $(varP 'N.constructor) = $(conE conName)
                 $(varP 'N.field) = $(lam "x" (\x -> letCon1 conName "y" x (\y -> y))) |]

  i <- do
    body' <- body
    instanceD (pure [])
                 [t| N.Newtype $(conT tyName) |]
                 (map pure body')
  pure [i]

data ConTysFields = ConTys   Int
                  -- ^^ The number of constructor fields
                  | FieldTys [Name]
                  -- ^^ The fieldname of each constructor field

lengthCons :: ConTysFields -> Int
lengthCons (ConTys n)   = n
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
    return DataDecStuff { dTyName  = tyName
                        , dTyVars  = map tvName tyVars
                        , dConName = conName
                        , dConTys  = conTys
                        }

dataDecStuffOfInfo (TyConI (NewtypeD _cxt tyName tyVars _kind constructor _deriving)) =
  do
    (conName, conTys) <- extractConstructorStuff [constructor]
    return DataDecStuff { dTyName  = tyName
                        , dTyVars  = map tvName tyVars
                        , dConName = conName
                        , dConTys  = conTys
                        }
dataDecStuffOfInfo _ = Left "That doesn't look like a data or newtype declaration to me"

conStuffOfConstructor :: Con -> Either Error (Name, ConTysFields)
conStuffOfConstructor = \case
  NormalC conName st -> return (conName, ConTys (length st))
  RecC conName vst -> return (conName, FieldTys (map (\(n, _, _) -> n) vst))
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
        before = foldl (\x y -> [t| $x $(pApp y) |]) (conT tyName') tyVars
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

adaptorDefinition :: Int -> Name -> Name -> Q [Dec]
adaptorDefinition numConVars conName x = do
  [d| $(varP x) = $(let_ "toTupleN" (pure $ toTuple conName numConVars)
                $ \toTupleN -> let_ "fromTuple" (pure $ fromTuple conName numConVars)
                $ \fromTupleN -> let_ "theDimap" [| $(varE 'dimap) $toTupleN $fromTupleN |]
                $ \theDimapN -> [| $theDimapN . $pN . $toTupleN |] ) |]

  where pN = varE (tupleAdaptors numConVars)

type MExp = forall m. Monad m => m Exp

lam :: String -> (MExp -> Q Exp) -> Q Exp
lam n f = do
  x <- newName n
  [| \ $(varP x) -> $(f (pure (VarE x))) |]

let_ :: String -> Q Exp -> (MExp -> Q Exp) -> Q Exp
let_ n rhs body = do
  x <- newName n
  [| let $(varP x) = $rhs in $(body (pure (VarE x))) |]

letCon1 :: Name -> String -> Q Exp -> (MExp -> Q Exp) -> Q Exp
letCon1 conName n rhs f = do
  x <- newName n
  [| let $(pure $ conP conName [VarP x]) = $rhs in $(f (pure (VarE x))) |]

adaptorDefinitionFields :: Name -> [Name] -> Name -> Q [Dec]
adaptorDefinitionFields conName fields adaptorName = do
  [d| $(varP adaptorName) = $(lam "f" body) |]
  where body :: MExp -> Q Exp
        body fE = case fields of
          []             -> error "Can't handle no fields in constructor"
          field1:fields' ->
            let first =
                  [| $(varE '(***$)) $(conE conName) $(theLmap field1 fE) |]
                app x y =
                  [| $(varE '(****)) $x $(theLmap y fE) |]
            in foldl app first fields'

        theLmap field fE =
          [| $(varE 'lmap) $(varE field) ($(varE field) $fE) |]

xTuple' :: ([Pat] -> Pat) -> ([Exp] -> Exp) -> Int -> Exp
xTuple' patCon retCon numTyVars = expr
  where expr = LamE [pat] body
        pat = patCon varPats
        body = retCon varExps
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

fromTuple :: Name -> Int -> Exp
fromTuple conName numTyVars = expr
  where retCon = appEAll (ConE conName)
        expr = xTuple' tupP retCon numTyVars

toTuple :: Name -> Int -> Exp
toTuple conName numTyVars = expr
  where patCon = conP conName
        expr = xTuple' patCon tupE numTyVars

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
