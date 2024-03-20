module BizExpr (repl, eval) where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

--import Data.Text as T

repl :: IO ()
repl = getLine >>= putStrLn . maybe "Failed to evaluate expression" show . eval >> repl

eval :: String -> Maybe Integer
eval x = coerceTo =<< evalAst . head . fst =<< parseLevel x

data Value = IntVal Integer | StrVal String | BoolVal Bool
  deriving (Show)

class CoerceTo a where
  coerceTo :: Value -> Maybe a
  coerceFrom :: a -> Value

instance CoerceTo Integer where
  coerceTo (IntVal n) = Just n
  coerceTo (StrVal s) = readMaybe s
  coerceTo (BoolVal True) = Just 1
  coerceTo (BoolVal False) = Just 0

  coerceFrom = IntVal

instance CoerceTo Bool where
  coerceTo (BoolVal v) = Just v
  coerceTo (IntVal v) = Just $ v /= 0
  coerceTo (StrVal "True") = Just True
  coerceTo (StrVal "true") = Just True
  coerceTo v = coerceTo . IntVal =<< (coerceTo v :: Maybe Integer) :: Maybe Bool

  coerceFrom = BoolVal

type Name = String

data Ast
  = Expr Name [Ast]
  | Val Value
  deriving (Show)

parseLevel :: String -> Maybe ([Ast], String)
parseLevel = go "" (Just [])
  where
    go :: String -> Maybe [Ast] -> String -> Maybe ([Ast], String)
    go prev (Just l) ('(' : next) = case parseLevel next of
      Nothing ->
        Nothing
      Just (arg, remnant) ->
        let r = if remnant /= "" && head remnant == ',' then tail remnant else remnant
         in go "" (Just (l ++ [Expr prev arg])) r
    go prev (Just l) (')' : remnant) =
      let l0 = [Val $ StrVal prev | prev /= ""]
       in Just (l ++ l0, remnant)
    go prev (Just l) (',' : next) =
      let l0 = [Val $ StrVal prev]
       in go "" (Just (l ++ l0)) next
    go "" (Just l) "" =
      Just (l, "")
    go prev (Just l) "" =
      Just (l ++ [Val $ StrVal prev], "")
    go prev l (x : xs) =
      go (prev ++ [x]) l xs

evalAst :: Ast -> Maybe Value
evalAst (Expr name ast) = fromMaybe Nothing (getFunc name <*> evalLevel ast)
evalAst (Val val) = Just val

evalLevel :: [Ast] -> Maybe [Value]
evalLevel = mapM evalAst

getFunc :: String -> Maybe ([Value] -> Maybe Value)
getFunc "sum" = Just eSum
getFunc "any" = Just eAny
getFunc "all" = Just eAll
getFunc _ = Nothing

eSum :: [Value] -> Maybe Value
eSum = eFold (+) (0 :: Integer)

eAny :: [Value] -> Maybe Value
eAny = eFold (||) False

eAll :: [Value] -> Maybe Value
eAll = eFold (&&) True

eFold :: (CoerceTo a, CoerceTo b) => (b -> a -> b) -> b -> [Value] -> Maybe Value
eFold fn init [] = Just $ coerceFrom init
eFold fn init (x : xs) =
  let b = coerceTo =<< eFold fn init xs
      a = coerceTo x
   in coerceFrom <$> (fn <$> b <*> a)
