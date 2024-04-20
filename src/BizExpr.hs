{-# LANGUAGE FlexibleInstances #-}

module BizExpr (repl, eval, Context) where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Data.Text.Lazy as L
import qualified Data.Map as M
import System.IO (isEOF)
import Control.Monad (when)

type Context = M.Map L.Text L.Text

repl :: IO ()
repl = do
  eof <- isEOF
  when (not eof) (
      getLine >>=
        putStrLn
        . maybe "Failed to evaluate expression" id
        . (eval M.empty :: String -> Maybe String)
      >> repl
    )

maybeHead (x:_) = Just x
maybeHead _ = Nothing

eval :: CoerceTo a => Context -> String -> Maybe a
eval c x = coerceTo =<< evalAst =<< maybeHead . fst =<< parseLevel c x

data Value = IntVal Integer | StrVal String | BoolVal Bool | FloatVal Float
  deriving (Show)

class CoerceTo a where
  coerceTo :: Value -> Maybe a
  coerceFrom :: a -> Value

instance CoerceTo Integer where
  coerceTo (FloatVal n) = Just $ floor n
  coerceTo (IntVal n) = Just n
  coerceTo v = coerceTo . FloatVal =<< coerceTo v
  coerceFrom = IntVal

instance CoerceTo Float where
  coerceTo (FloatVal n) = Just n
  coerceTo (IntVal n) = Just $ fromIntegral n
  coerceTo (StrVal s) = readMaybe s
  coerceTo (BoolVal True) = Just 1
  coerceTo (BoolVal False) = Just 0

  coerceFrom = FloatVal

instance CoerceTo String where
  coerceTo (BoolVal v) = Just $ show v
  coerceTo (IntVal v) = Just $ show v
  coerceTo (StrVal v) = Just v
  coerceTo (FloatVal v) = Just $ show v
  coerceFrom = StrVal

instance CoerceTo Bool where
  coerceTo (BoolVal v) = Just v
  coerceTo (IntVal v) = Just $ v /= 0
  coerceTo (StrVal "True") = Just True
  coerceTo (StrVal "true") = Just True
  coerceTo v = coerceTo . IntVal =<< coerceTo v

  coerceFrom = BoolVal

type Name = String

data Ast
  = Expr Name [Ast]
  | Val Value
  deriving (Show)

parseLevel :: Context -> String -> Maybe ([Ast], String)
parseLevel c = go "" (Just [])
  where
    go :: String -> Maybe [Ast] -> String -> Maybe ([Ast], String)
    go prev (Just l) ('(' : next) =
      case parseLevel c next of
        Nothing ->
          Nothing
        Just (arg, remnant) ->
          let r =
                if remnant /= "" && head remnant == ','
                  then tail remnant
                  else remnant
           in go "" (Just (l ++ [Expr prev arg])) r
    go prev (Just l) (')' : remnant) =
      let l0 = [Val $ newVal c prev | prev /= ""]
       in Just (l ++ l0, remnant)
    go prev (Just l) (',' : next) =
      let l0 = [Val $ newVal c prev]
       in go "" (Just (l ++ l0)) next
    go prev l (' ' : next) =
      go "" l next
    go "" (Just l) "" =
      Just (l, "")
    go prev (Just l) "" =
      Just (l ++ [Val $ newVal c prev], "")
    go prev l (x : xs) =
      go (prev ++ [x]) l xs

newVal :: Context -> String -> Value
newVal c x = case M.lookup (L.pack x) c of
  Nothing -> StrVal x
  Just n -> StrVal $ L.unpack n

evalAst :: Ast -> Maybe Value
evalAst (Expr name ast) = fromMaybe Nothing (getFunc name <*> evalLevel ast)
evalAst (Val val) = Just val

evalLevel :: [Ast] -> Maybe [Value]
evalLevel = mapM evalAst

getFunc :: String -> Maybe ([Value] -> Maybe Value)
getFunc "sum" = Just eSum
getFunc "any" = Just eAny
getFunc "all" = Just eAll
getFunc "average" = Just eAverage
getFunc "floor" = Just eFloor
getFunc "not" = Just eNot
getFunc _ = Nothing

eFloor :: [Value] -> Maybe Value
eFloor (v:_) = IntVal <$> (floor <$> (coerceTo v :: Maybe Float))

eNot :: [Value] -> Maybe Value
eNot (v:_) = BoolVal <$> (not <$> coerceTo v)

eSum :: [Value] -> Maybe Value
eSum = eFold (+) (0.0 :: Float)

eAny :: [Value] -> Maybe Value
eAny = eFold (||) False

eAll :: [Value] -> Maybe Value
eAll = eFold (&&) True

eAverage :: [Value] -> Maybe Value
eAverage xs = do
  let n = coerceTo =<< eSum xs
      d = Just . fromIntegral $ length xs
   in FloatVal <$> ((/) <$> n <*> d)

eFold :: (CoerceTo a, CoerceTo b) => (b -> a -> b) -> b -> [Value] -> Maybe Value
eFold fn init [] = Just $ coerceFrom init
eFold fn init (x : xs) =
  let b = coerceTo =<< eFold fn init xs
      a = coerceTo x
   in coerceFrom <$> (fn <$> b <*> a)
