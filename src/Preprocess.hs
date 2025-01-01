{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Preprocess ( 
    fillRands, typeDepth, expDepth, expSize, uniquefyVars, simplifyExp
) where

import Syntax.Grammar.Abs ( Exp(..), Type(..), Ident(..) )
import Syntax.AbsF

#ifdef DEBUG
import Debug.Trace ( trace )
#else
import Debug.NoTrace ( trace )
#endif
import Control.Monad.Reader ( Reader, runReader, MonadReader (local), asks )
import Data.HashMap.Lazy ( HashMap, empty, findWithDefault, insertWith )
import Data.Binary.Get ( runGet, getInt64host )
import Data.ByteString.Lazy.Char8 ( pack )
import Control.Monad.Random
import Data.Functor.Foldable.Monadic ( anaM, apoM )
import Data.Functor.Foldable( project, Recursive(cata))
import Data.Functor ( (<&>) )
import Control.Monad.State (StateT, MonadState, evalStateT, modify, gets)
import Data.List.Split (splitOn)
import Eval (sqrtPos, expUnit, divUnit, modUnit, addUnit, subUnit, substitute)

-- |The depth of the expression (height of the tree)
expDepth :: Exp -> Int
expDepth = cata go where go other = 1 + foldr max 0 other

typeDepth :: Type -> Int
typeDepth = cata go where go other = 1 + foldr max 0 other

-- |The size of the expression (number of nodes)
expSize :: Exp -> Int
expSize = cata go where
    go :: ExpF Int -> Int
    go other = 1 + sum other

-- |Replace all the `Rand` occurences with a random double (seeded with `seed`)
fillRands :: String -> Exp -> Exp
fillRands seed e = do
    let intFromHash s = fromIntegral $ runGet getInt64host (pack s)
    let g = mkStdGen $ intFromHash seed
    evalRand (fillRandsM e) g

fillRandsM :: Exp -> Rand StdGen Exp
fillRandsM = anaM go where
    go Rand = getRandomR (-1, 1) <&> DValF
    go other = return (project other)

-- This map contains, for each variable, the amount of variables that already 
-- have that name. We can thus safely rename this variable to n+1 (shadowed)
type SubMap = HashMap String [Int]
-- This map contains, for each variable, the amount of variables of that name
-- we have already encountered, allowing incremental renaming only of collisions
type DepthMap = HashMap String Int
-- Monad containing above data to perform variable renaming under
newtype AnnotateMonad a = AnnotateMonad {
    annotateMonad :: StateT DepthMap (Reader SubMap) a
} deriving ( Functor, Applicative, Monad, 
             MonadState DepthMap, MonadReader SubMap )

runAnnotate :: AnnotateMonad a -> a
runAnnotate m = do
    let state = annotateMonad m
    let reader = evalStateT state empty
    runReader reader empty

uniquefyVars :: Exp -> Exp
uniquefyVars e = runAnnotate $ uniquefyVarsM e

-- Increments counter and pushes new substitute for x onto m[x]
pushVar :: Ident -> Int -> SubMap -> SubMap
pushVar (Ident x) c = insertWith (++) x [c]

-- Gets substitute for x by consulting the VarMap in the reader
-- Introduces the (illegal) `:` character, so no collisions should be possible
getSub :: Ident -> SubMap -> Ident
getSub (Ident x) m = do 
    let base = case splitOn ":" x of
            (b:_) -> b
            _ -> error "Split should always have one element right?"
    Ident $ base ++ ":" ++ show (head $ findWithDefault [0] x m)

-- TODO: add match binders
uniquefyVarsM :: Exp -> AnnotateMonad Exp
uniquefyVarsM = apoM doSub where
    doSub :: Exp -> AnnotateMonad (ExpF (Either Exp Exp))
    doSub (Var x) = asks (VarF . getSub x)
    doSub (Abstr i@(Ident x) body) = do
        modify (insertWith (+) x 1)
        depth <- gets (findWithDefault 0 x)
        subbedIdent <- asks (getSub i . pushVar i depth)
        subbedBody <- local (pushVar i depth) $ uniquefyVarsM body
        return $ Left <$> AbstrF subbedIdent subbedBody
    doSub other = return $ Right <$> project other

inferType :: Exp -> Type
inferType e = case e of
    BTrue -> TBool
    BFalse -> TBool
    Rand -> TDouble
    DVal _ -> TDouble
    Min _ -> TDouble
    Sqrt _ -> TDouble
    Sin _ -> TDouble
    Cos _ -> TDouble
    EPow _ -> TDouble
    Mul _ _ -> TDouble
    Div _ _ -> TDouble
    Mod _ _ -> TDouble
    Add _ _ -> TDouble
    Sub _ _ -> TDouble
    Eq _ _ -> TBool
    Lt _ _ -> TBool
    Gt _ _ -> TBool
    other -> error $ "cannot infer type of " ++ show other

applyDouble2
    :: (Exp -> Exp -> Exp)          -- Default if args were not doubles
    -> (Double -> Double -> Double) -- Function to apply if 2 doubles
    -> Exp -> Exp                   -- Two expressions to simplify
    -> Exp                          -- Potentially simplified expression
applyDouble2 _ f (DVal a) (DVal b) = DVal (f a b)
applyDouble2 default' _ a b = default' a b

applyToDouble :: (Exp -> Exp) -> (Double -> Double) -> Exp -> Exp
applyToDouble _ f (DVal a) = DVal (f a)
applyToDouble default' _ a = default' a

fromHsBool :: Bool -> Exp 
fromHsBool True = BTrue
fromHsBool False = BFalse

applyComp
    :: (Exp -> Exp -> Exp)          -- Default if args were not doubles
    -> (Double -> Double -> Bool)   -- Function to apply if 2 doubles
    -> Exp -> Exp                   -- Two expressions to evaluate
    -> Exp                          -- Simplified exp
applyComp _ f (DVal a) (DVal b) = fromHsBool $ f a b
applyComp default' _ a b = default' a b

tryAnd :: Exp -> Exp -> Exp
-- Actual truth table
tryAnd BTrue BTrue = BTrue
tryAnd BFalse BFalse = BFalse
tryAnd BTrue BFalse = BFalse
tryAnd BFalse BTrue = BFalse
-- And Identity laws
tryAnd BFalse _ = BFalse
tryAnd _ BFalse = BFalse
tryAnd a BTrue = a
tryAnd BTrue a = a
-- No simplification possible
tryAnd a b = And a b

tryOr :: Exp -> Exp -> Exp
-- Actual truth table
tryOr BTrue BTrue = BTrue
tryOr BFalse BFalse = BFalse
tryOr BTrue BFalse = BTrue
tryOr BFalse BTrue = BTrue
-- Or Identity laws
tryOr BTrue _ = BTrue
tryOr _ BTrue = BTrue
tryOr a BFalse = a
tryOr BFalse a = a
-- No simplification possible
tryOr a b = And a b

tryNot :: Exp -> Exp
tryNot BFalse = BTrue
tryNot BTrue = BFalse
tryNot a = Not a

-- Or|Simplify an expression by evaluating all subtrees that evaluate to a number
-- Orin advance, saving time in evaluation which is done millions of times
simplifyExp :: Exp -> Exp
simplifyExp e = case e of
    -- No simplificaiton possible, explicitly list
    Rand -> e
    BTrue -> e
    BFalse -> e
    Var _ -> e
    DVal _ -> e
    InL _ -> e
    InR _ -> e
    Tup _ _ -> e
    -- Arithmetic operators
    (Min a) -> applyToDouble Min negate (simplifyExp a)
    (Sqrt a) -> applyToDouble Sqrt sqrtPos (simplifyExp a)
    (Sin a) -> applyToDouble Sin sin (simplifyExp a)
    (Cos a) -> applyToDouble Cos cos (simplifyExp a)
    (EPow a) -> applyToDouble EPow expUnit (simplifyExp a)
    (Mul a b) -> applyDouble2 Mul (*) (simplifyExp a) (simplifyExp b)
    (Div a b) -> applyDouble2 Div divUnit (simplifyExp a) (simplifyExp b)
    (Mod a b) -> applyDouble2 Mod modUnit (simplifyExp a) (simplifyExp b)
    (Add a b) -> applyDouble2 Add addUnit (simplifyExp a) (simplifyExp b)
    (Sub a b) -> applyDouble2 Sub subUnit (simplifyExp a) (simplifyExp b)
    -- Comparison operators
    (Eq a b) -> applyComp Eq (==) (simplifyExp a) (simplifyExp b)
    (Lt a b) -> applyComp Lt (<) (simplifyExp a) (simplifyExp b)
    (Gt a b) -> applyComp Gt (>) (simplifyExp a) (simplifyExp b)
    (Neq a b) -> applyComp Neq (/=) (simplifyExp a) (simplifyExp b)
    (Leq a b) -> applyComp Leq (<=) (simplifyExp a) (simplifyExp b)
    (Geq a b) -> applyComp Geq (>=) (simplifyExp a) (simplifyExp b)
    -- Boolean operators
    (And a b) -> tryAnd (simplifyExp a) (simplifyExp b)
    (Or a b) -> tryOr (simplifyExp a) (simplifyExp b)
    (Not a) -> tryNot (simplifyExp a)

    (Match m x1 e1 x2 e2) -> case simplifyExp m of
        (InL r) -> simplifyExp $ substitute x1 e1 r
        (InR r) -> simplifyExp $ substitute x2 e2 r
        other -> Match other x1 (simplifyExp e1) x2 (simplifyExp e2)
    Abstr a b -> Abstr a $ simplifyExp b
    (App a b) -> App (simplifyExp a) (simplifyExp b)
    (Fst a) -> case simplifyExp a of
        (Tup ra _) -> ra
        other -> Fst other
    (Snd a) -> case simplifyExp a of
        (Tup _ rb) -> rb
        other -> Snd other
    (Ite a b c) -> case simplifyExp a of
        BTrue -> simplifyExp b 
        BFalse -> simplifyExp c
        bexp -> Ite bexp (simplifyExp b) (simplifyExp c)
    -- other -> other
