module Value ( Value (..), valToExp ) where

import Syntax.Grammar.Abs (Exp (..), Ident)

data Value
    = VVal Double
    | VBVal Bool
    | VPair Value Value
    | VL Value
    | VR Value
    | VFun Ident Exp 
    deriving (Eq, Ord, Show, Read)

valToExp :: Value -> Exp
valToExp (VVal d) = DVal d
valToExp (VBVal True) = BTrue
valToExp (VBVal False) = BFalse
valToExp (VPair a b) = Tup (valToExp a) (valToExp b)
valToExp (VL a) = InL $ valToExp a
valToExp (VR a) = InR $ valToExp a
valToExp (VFun i e) = Abstr i e
