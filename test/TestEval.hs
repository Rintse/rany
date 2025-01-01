module TestEval (testEval) where

import Eval (evalExpM)
import Value (Value (..))
import Syntax.Parse (parseExp)
import Control.Monad.Except (runExcept)

data TestProgram = TestProgram {
    name :: String,
    content :: String,
    expected :: Value
} deriving (Show)

programs :: [TestProgram]
programs =
    [ TestProgram
        "Simple function application"
        "(lam x -> x) 1.0"
        (VVal 1.0)
    , TestProgram
        "If then else"
        "(lam x -> if (x < 0.5) then 0.0 else 1.0) 0.25"
        (VVal 0.0)
    , TestProgram
        "Higher order variables"
        "( ( lam f -> ( lam x -> f (f x) ) ) ( lam x -> x + 0.4 ) ) 0.6"
        (VVal 0.45)
    ]

doTest :: TestProgram -> IO Bool
doTest TestProgram { name=n, content=c, expected=e } = do
    putStr $ "Running test: " ++ show n ++ "... "
    ast <- parseExp c
    case runExcept $ evalExpM ast of
        Left s -> do
            putStrLn ("Error testing: " ++ s)
            return False
        Right value | value == e -> do
            putStrLn "Success."
            return True
        Right other -> do
            putStrLn "Faulty result."
            putStrLn $ "Expected: " ++ show e
            putStrLn $ "Got: " ++ show other
            return False

testEval :: IO Bool
testEval = and <$> mapM doTest programs
