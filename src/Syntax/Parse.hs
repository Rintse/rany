module Syntax.Parse ( parseExp, parseType ) where

import Data.Functor.Foldable ( cata )
import Preprocess ( uniquefyVars )
import Syntax.Grammar.Par
import Syntax.Grammar.Abs
import Syntax.AbsF
import System.Exit

getInvalidDoubles :: Exp -> [Double]
getInvalidDoubles = cata go where
    go (DValF d) = [d | d < -1 || d > 1]
    go other = concat other

-- Parses contents of given input file
parseExp :: String -> IO Exp
parseExp s = do
    let ts = myLexer s
    case pExp ts of
        Left err_msg -> do
            putStrLn $ "Expression parse failed: " ++ err_msg
            putStrLn $ "Tokens still in stream:\n" ++ show ts
            exitFailure
        Right expr -> do
            case getInvalidDoubles expr of
                [] -> return $ uniquefyVars expr
                non_empty_list -> do
                    putStrLn $ "Parsed expression contains numbers outside of"
                        ++ "the [-1, 1] range: " ++ show non_empty_list
                    exitFailure

parseType :: String -> IO Type
parseType s = do
    let ts = myLexer s
    case pType ts of
        Left err_msg -> do
            putStrLn $ "Type parse failed: " ++ err_msg
            putStrLn $ "Tokens still in stream:\n" ++ show ts
            exitFailure
        Right triple -> return triple
