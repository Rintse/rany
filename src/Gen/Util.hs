{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gen.Util (
    GenMonad(..), BoundVar(..), pickVar, withNewVar, newVarName, 
    pickWeighted, pickWeightedG, trace
) where

-- Re-export trace here once such that we can have just one ifdef
#ifdef DEBUG
import Debug.Trace ( trace )
#else
import Debug.NoTrace ( trace )
#endif

import Syntax.Grammar.Abs (Type, Exp(..), Ident(..))
import QuickCheck.GenT (GenT, MonadGen, choose)
import Control.Monad.Reader (Reader, MonadReader, asks)

-- |Bound variables have a name and type, but also a weight, to be able 
-- |to give higher chances of generating more inner variables
data BoundVar = BoundVar {
    weight :: Double,
    name :: String,
    typ :: Type
} deriving (Show, Eq, Ord, Read)

-- |The factor to scale weights of existing vars when new binder is added
-- |This makes the variables with closer binders more likely to be generated
varScale :: Double
varScale = 0.8

-- |Insert new variable and scale all the existing ones
withNewVar :: String -> Type -> [BoundVar] -> [BoundVar]
withNewVar n t = (BoundVar {weight=1, name=n, typ=t} :) . map scaleVar where
    scaleVar v = v { weight = weight v * varScale }

-- |Pick a random generator in a weighted list under the generation monad
pickWeightedG :: [(Int, GenMonad a)] -> GenMonad a
pickWeightedG l = do
    choose (0, sum (map fst l) -1) >>= (`select` l) where
    select n ((k,x):xs)
        | n <= k    = x
        | otherwise = select (n-k) xs
    select _ _  = error "select on empty list"

-- |Pick a random element in a weighted list
pickWeighted :: [(Int, a)] -> GenMonad a
pickWeighted l = do
    choose (0, sum (map fst l) -1) >>= (`select` l) where
    select n ((k,x):xs)
        | n <= k    = return x
        | otherwise = select (n-k) xs
    select _ _  = error "select on empty list"

-- |Get arbitrary variable identifier of the requested type
pickVar :: [BoundVar] -> GenMonad Exp
pickVar vars = do
    let weightedVars = map (\x -> (round (weight x * 100) , x)) vars
    Var . Ident . name <$> pickWeighted weightedVars

-- |A new variable name that comes after all the existing variables
newVarName :: GenMonad String
newVarName = asks ((("x" ++) . show) . length)

-- |Generation monad that tracks the currently bound variables in a state monad
newtype GenMonad a = EvalMonad {
    genMonad :: GenT (Reader [BoundVar]) a
} deriving  ( Functor, Applicative, Monad
            , MonadReader [BoundVar], MonadGen )
