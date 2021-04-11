module MyLib (someFunc) where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

--import Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"

repl :: IO ()
repl = getLine >>= putStrLn . maybe "Failed to evaluate expression" show . eval >> repl


eval :: String -> Maybe Integer
eval x = coerceToInt =<< evalAst.head.fst =<< parseLevel x


data Value = IntVal Integer | StrVal String
  deriving (Show)

coerceToInt :: Value -> Maybe Integer
coerceToInt (IntVal n) = Just n
coerceToInt (StrVal s) = readMaybe s

type Name = String

data Ast
  = Expr Name [Ast]
  | Val Value
  deriving (Show)

parseLevel :: String -> Maybe ([Ast], String)
parseLevel = go "" (Just [])
  where
    go :: String -> Maybe [Ast] -> String -> Maybe ([Ast], String)
    go prev (Just l) ('(':next) = case parseLevel next of
      Nothing ->
        Nothing
      Just (arg, remnant) ->
        let 
          r = if remnant /= "" && head remnant == ',' then tail remnant else remnant
        in
          go "" (Just (l ++ [Expr prev arg])) r
    go prev (Just l) (')':remnant) =
      let
        l0 = [Val $ StrVal prev | prev /= ""]
      in
        Just (l ++ l0, remnant)
    go prev (Just l) (',':next) =
      let
        l0 = [Val $ StrVal prev]
      in
        go "" (Just (l ++ l0)) next
    go "" (Just l) "" = 
      Just (l, "")
    go prev (Just l) "" =
      Just (l ++ [Val $ StrVal prev], "")
    go prev l (x:xs) =
      go (prev ++ [x]) l xs

evalAst :: Ast -> Maybe Value
evalAst (Expr name ast) = fromMaybe Nothing (getFunc name <*> evalLevel ast)
evalAst (Val val) = Just val

evalLevel :: [Ast] -> Maybe [Value]
evalLevel = mapM evalAst


getFunc :: String -> Maybe ([Value] -> Maybe Value)
getFunc _ = Just eSum


-- (name, (func, minParams))
funcs0 = [("sum",(eSum, 0))]

eSum :: [Value] -> Maybe Value
eSum [] = Just $ IntVal 0
eSum (x:xs) = 
    let
      a1 = coerceToInt =<< eSum xs
      a0 = coerceToInt x
    in
      IntVal <$> ((+) <$> a1 <*> a0)

