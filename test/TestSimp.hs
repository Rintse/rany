module TestSimp (testSimp) where

import Eval (evalExpM)
import Gen.Exp (genExp)
import Syntax.Grammar.Abs (Type(TDouble), Exp)
import Control.Monad.Random (randomIO, replicateM)
import Preprocess (simplifyExp, fillRands)
import Control.Monad.Except (runExcept)
import Data.Either (lefts)
import System.Exit (exitFailure)

numTests :: Int
numTests = 1000

doTest :: Exp -> IO Bool
doTest e = do
    let simplified = simplifyExp e
    let r = runExcept $ evalExpM e
    let rSimp = runExcept $ evalExpM simplified
    case (r, rSimp) of
        (Right v1, Right v2) | v1 == v2 -> return True
        (Right v1, Right v2) -> do
            putStrLn "Simplified expression did not yield identical resutls:"
            putStrLn $ "Expected value: " ++ show v1
            putStrLn $ "Simplified value: " ++ show v2
            return False
        (a, b) -> do
            putStrLn "Error(s) running test:"
            mapM_ (putStrLn . ("  - " ++)) $ lefts [a, b]
            return False

testSimp :: IO ()
testSimp = do
    seeds <- replicateM numTests $ replicateM 8 (randomIO :: IO Char)
    toTest <- mapM (\s -> fmap (fillRands s) (genExp TDouble s)) seeds
    success <- and <$> mapM doTest toTest

    if success 
        then putStrLn "Simplification tests succesfull."
        else putStrLn "Simplification tests failed" >> exitFailure
