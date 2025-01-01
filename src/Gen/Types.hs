module Gen.Types (genType) where

import Syntax.Grammar.Abs (Type(..))
import Gen.Util (GenMonad, pickWeightedG, trace)

import QuickCheck.GenT (resize, sized)

genType :: GenMonad Type
genType = sized go where
    go :: Int -> GenMonad Type
    go size = do
        let genT = resize (size - 1) genType
        let nonLeafs =
                [ (100, liftA2 TFun genT genT)
                , (100, liftA2 TProd genT genT)
                ]
        let leafs =
                [ (100, pure TDouble)
                , (100, pure TBool)
                ]
        let allNodes = nonLeafs ++ leafs
        trace ("genType [size = " ++ show size ++ "]")
            $ if size <= 0
                then pickWeightedG leafs
                else  pickWeightedG allNodes
