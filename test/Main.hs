{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main (main) where

import TestEval (testEval)
import TestSimp (testSimp)
import System.Exit (exitFailure)

main :: IO ()
main = do
    success <- testEval
    if success 
        then putStrLn "Eval tests succesfull."
        else putStrLn "Eval tests failed" >> exitFailure

    success <- testSimp
    if success 
        then putStrLn "Simplification tests succesfull."
        else putStrLn "Simplification tests failed" >> exitFailure
