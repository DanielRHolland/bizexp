module Main where

import qualified BizExpr (eval, repl)

main :: IO ()
main = BizExpr.repl
