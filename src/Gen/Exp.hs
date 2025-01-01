{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
-- TODO: make all of these not orphaned instances
{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE FlexibleInstances #-}

module Gen.Exp (genExp) where

import Syntax.Grammar.Abs
import Syntax.Grammar.Print ( printTree )
import Gen.Types ( genType )
import Gen.Util ( GenMonad(..), newVarName, withNewVar, BoundVar(..), pickVar
                , pickWeightedG, trace )
import qualified Test.QuickCheck as QC ( Arbitrary, arbitrary, frequency, Gen
                                       , resize )
import QuickCheck.GenT ( MonadGen ( liftGen ), runGenT, GenT, MonadGen
                       , getSize, resize, oneof, sized, choose )
import Control.Monad.Reader ( Reader, runReader, MonadTrans ( lift )
                            , ReaderT (runReaderT, ReaderT)
                            , MonadReader (local, ask), asks )
import Generic.Random
import Data.HashMap.Lazy ( HashMap, empty )
import GHC.Generics ( Generic )
import Data.Binary.Get ( runGet, getInt64host )
import Data.ByteString.Lazy.Char8 ( pack )
import Test.QuickCheck.Random ( mkQCGen, QCGen )
import Control.Monad.Identity (Identity)
import Data.Bifunctor (Bifunctor(second, first))
import Test.QuickCheck.Gen (Gen(unGen))
import Preprocess ( typeDepth )
import Control.Applicative (liftA3)
import Test.QuickCheck (Args(maxSize))

maxRec :: Int
maxRec = 5
minRec :: Int
minRec = 3

-- TODO: dont generate (fst tup) etc when there are no variables in scope

genOfType' :: Type -> GenMonad Exp
genOfType' t@TDouble = do
    size <- getSize
    -- TODO: this type is now with depth 1/5th of the remaining budget..? 
    -- make this choice non-arbitrary somehow?
    randomType <- resize 0 genType
    let genDouble = resize (size - 1) $ genOfType TDouble
    let genBool = resize (size - 1) $ genOfType TBool
    let genFunction = resize (size - 1) $ genOfType (TFun randomType t)
    let genArg = resize (size - 1) $ genOfType randomType
    let genLeft = resize (size - 1) $ genOfType (TProd t randomType)
    let genRight = resize (size - 1) $ genOfType (TProd randomType t)
    validVars <- asks (filter ((==t) . typ))

    -- All the ways (i can think of) to get to a double from other terms
    let nonLeafsG =
            [ (050, trace "Min"  $ fmap Min genDouble)
            , (050, trace "Sqrt" $ fmap Sqrt genDouble)
            , (050, trace "Sin"  $ fmap Sin genDouble)
            , (050, trace "Cos"  $ fmap Cos genDouble)
            , (050, trace "EPow" $ fmap EPow genDouble)
            , (050, trace "Mul"  $ liftA2 Mul genDouble genDouble)
            , (050, trace "Div"  $ liftA2 Div genDouble genDouble)
            , (050, trace "Mod"  $ liftA2 Mod genDouble genDouble)
            , (050, trace "Add"  $ liftA2 Add genDouble genDouble)
            , (050, trace "Sub"  $ liftA2 Sub genDouble genDouble)
            , (050, trace "Ite"  $ liftA3 Ite genBool genDouble genDouble)
            , (010, trace "App"  $ liftA2 App genFunction genArg)
            , (010, trace "Fst"  $ fmap Fst genLeft)
            , (010, trace "Snd"  $ fmap Snd genRight)
            ]
    let leafsG =
            [ (050, trace "DVal" $ DVal <$> choose (-1, 1))
            , (100, trace "Rand" $ return Rand)
            ] ++ 
            [ (200, trace "pickVar" $ pickVar validVars) | not (null validVars) ]
    let allNodesG = nonLeafsG ++ leafsG

    notInFunc <- asks null
    if | notInFunc -> pickWeightedG leafsG
       | size >= maxRec - minRec -> pickWeightedG nonLeafsG
       | size >= 0 && size >= maxRec - minRec -> pickWeightedG allNodesG
       | otherwise -> pickWeightedG leafsG


genOfType' t@TBool = do
    size <- getSize
    randomType <- resize 0 genType
    let genDouble = resize (size - 1) $ genOfType TDouble
    let genBool = resize (size - 1) $ genOfType TBool
    let genFunction = resize (size - 1) $ genOfType (TFun randomType t)
    let genArg = resize (size - 1) $ genOfType randomType
    let genLeft = resize (size - 1) $ genOfType (TProd t randomType)
    let genRight = resize (size - 1) $ genOfType (TProd randomType t)
    validVars <- asks (filter ((==TBool) . typ))

    let stalksG =
            [ (050, trace "Eq"  $ liftA2 Eq genDouble genDouble)
            , (050, trace "Lt"  $ liftA2 Lt genDouble genDouble)
            , (050, trace "Gt"  $ liftA2 Gt genDouble genDouble)
            , (050, trace "Neq" $ liftA2 Neq genDouble genDouble)
            , (050, trace "Leq" $ liftA2 Leq genDouble genDouble)
            , (050, trace "Geq" $ liftA2 Geq genDouble genDouble)
            ] ++ 
            [ (100, trace "Var" $ pickVar validVars) | not (null validVars) ]
    let nonStalksG =
            [ (100, trace "Not" $ fmap Not genBool)
            , (100, trace "And" $ liftA2 And genBool genBool)
            , (100, trace "Or"  $ liftA2 Or genBool genBool)
            , (050, trace "Fst" $ fmap Fst genLeft)
            , (050, trace "Snd" $ fmap Snd genRight)
            , (050, trace "App" $ liftA2 App genFunction genArg)
            , (050, trace "Ite" $ liftA3 Ite genBool genBool genBool)
            ]
    let notInFuncG = 
            [ (100, trace "BTrue"  $ return BTrue) 
            , (100, trace "BFalse" $ return BFalse) ]

    notInFunc <- asks null
    if | notInFunc -> pickWeightedG notInFuncG
       | size >= 1 -> pickWeightedG nonStalksG
       | otherwise -> pickWeightedG stalksG


genOfType' t@(TProd a b) = do
    size <- getSize
    randomType <- resize 0 genType
    let genFunction = resize (size - 1) $ genOfType (TFun randomType t)
    let genArg = resize (size - 1) $ genOfType randomType
    let genLeft = resize (size - 1) $ genOfType (TProd t randomType)
    let genRight = resize (size - 1) $ genOfType (TProd randomType t)
    let genBool = resize (size - 1) $ genOfType TBool
    let genSelf = resize (size - 1) $ genOfType t
    validVars <- asks (filter ((==t) . typ))

    -- Products can also be a tuple of the input types
    let genA = resize (size - 1) $ genOfType a
    let genB = resize (size - 1) $ genOfType b

    let deeperG = 
            [ (020, trace "App" $ liftA2 App genFunction genArg)
            , (020, trace "Fst" $ fmap Fst genLeft)
            , (020, trace "Snd" $ fmap Snd genRight)
            , (020, trace "Ite" $ liftA3 Ite genBool genSelf genSelf)
            ]
    let termG = 
            [ (100, trace "Tup" $ liftA2 Tup genA genB ) ] ++
            [ (200, trace "Var" $ pickVar validVars) | not (null validVars) ]
    let notInFuncG = termG ++ [(020, App <$> genFunction <*> genArg)]
    let allG = deeperG ++ termG

    notInFunc <- asks null
    if | notInFunc -> pickWeightedG notInFuncG
       | size >= 1 -> pickWeightedG allG
       | otherwise -> pickWeightedG termG


genOfType' t@(TFun a b) = do
    size <- getSize
    randomType <- resize 0 genType
    let genFunction = resize (size - 1) $ genOfType (TFun randomType t)
    let genArg = resize (size - 1) $ genOfType randomType
    let genLeft = resize (size - 1) $ genOfType (TProd t randomType)
    let genRight = resize (size - 1) $ genOfType (TProd randomType t)
    let genBool = resize (size - 1) $ genOfType TBool
    let genSelf = resize (size - 1) $ genOfType t
    validVars <- asks (filter ((==t) . typ))

    -- Functions type terms can also be abstractions where the body is generated
    -- with the knowledge of the variable that was just introduced
    varName <- newVarName
    let bodyG = resize (size - 1) (genOfType b)
    let body = local (withNewVar varName a) bodyG

    let deeperG = 
            [ (020, trace "App" $ liftA2 App genFunction genArg)
            , (020, trace "Fst" $ fmap Fst genLeft)
            , (020, trace "Snd" $ fmap Snd genRight)
            , (020, trace "Ite" $ liftA3 Ite genBool genSelf genSelf )
            ]
    let termG = 
            [ (100, trace "Abstr" $ Abstr (Ident varName) <$> body) ] ++
            [ (100, trace "Var" $ pickVar validVars) | not (null validVars) ]
    let notInFuncG = termG
    let allG = deeperG ++ termG

    notInFunc <- asks null
    if | notInFunc -> pickWeightedG notInFuncG
       | size >= 1 -> pickWeightedG allG
       | otherwise -> pickWeightedG termG


genOfType' t@(TCoprod a b) = do
    size <- getSize
    randomType <- resize 0 genType
    let genLeft = resize (size - 1) $ genOfType (TProd t randomType)
    let genRight = resize (size - 1) $ genOfType (TProd randomType t)
    let genFunction = resize (size - 1) $ genOfType (TFun randomType t)
    let genArg = resize (size - 1) $ genOfType randomType
    let genSelf = resize (size - 1) $ genOfType t
    let genBool = resize (size - 1) $ genOfType TBool
    validVars <- asks (filter ((==t) . typ))
    
    -- Coproducts can also be injections of the input types
    let genA = resize (size - 1) $ genOfType a
    let genB = resize (size - 1) $ genOfType b

    let deeperG = 
            [ (020, trace "App" $ liftA2 App genFunction genArg)
            , (020, trace "Fst" $ fmap Fst genLeft)
            , (020, trace "Snd" $ fmap Snd genRight)
            , (020, trace "Ite" $ liftA3 Ite genBool genSelf genSelf )
            ]
    let termG = 
            [ (100, trace "InL" $ fmap InL genA )
            , (100, trace "InR" $ fmap InR genB ) ] ++
            [ (100, trace "Var" $ pickVar validVars) | not (null validVars) ]
    let notInFuncG = termG ++ [(020, liftA2 App genFunction genArg)]
    let allG = deeperG ++ termG

    notInFunc <- asks null -- no bounded variables
    if | notInFunc -> pickWeightedG notInFuncG
       | size >= maxRec - minRec -> pickWeightedG deeperG
       | size >= 0 && size >= maxRec - minRec -> pickWeightedG allG
       | otherwise -> pickWeightedG termG

-- Wrapper to print what is being generated 
genOfType :: Type -> GenMonad Exp
genOfType t = do
    size <- getSize
    trace ("genOfType(" ++ show t ++ ") [size = " ++ show size ++ "]") $ genOfType' t

-- |Generate a random expression of type `t` with rng seeded to `seed`
genExp :: Type -> String -> IO Exp
genExp t seed = do
    putStrLn "Generating a random expression of type:"
    putStrLn $ printTree t

    let intFromHash s = fromIntegral $ runGet getInt64host (pack s)
    let rng = mkQCGen $ intFromHash seed

    let gen = runGenT (genMonad $ genOfType t)
    let reader = unGen gen rng (maxRec + typeDepth t) 
    return $ runReader reader []
