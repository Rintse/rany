module Main (main) where

import TestEval (testEval)
import TestSimp (testSimp)

main :: IO ()
main = do
    testEval
    testSimp
