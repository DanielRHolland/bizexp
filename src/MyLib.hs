module MyLib (someFunc) where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

--import Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data FType = FList [FType] | FInt Int | FStr String | FNothing

type Func = FType -> FType

parseFunc :: String -> Maybe Func
parseFunc s =
  let pref = takeWhile (/= '(') s
      maybeFn = lookup pref funcs
   in fromMaybe Nothing maybeFn

--funcs = [("SUM", fSum)]
funcs = []

data Value = IntVal Integer | StrVal String
  deriving (Show)

coerceToInt :: Value -> Maybe Integer
coerceToInt (IntVal n) = Just n
coerceToInt (StrVal s) = readMaybe s

type Name = String

data Ast
  = Expr Name Ast
  | Val Value
  | Pair Ast Ast
  | Level [Ast]
  deriving (Show)

exprFuncs = ["sum", "any"]

--parseAst :: String -> Maybe Ast
--parseAst s
--  | validExpr = case parseAst suf of
--      Nothing -> Nothing
--      Just arg -> Just $ Expr pref arg
--  | validPair = case (parseAst pairL, parseAst pairR) of
--      (Nothing,_) -> Nothing
--      (_,Nothing) -> Nothing
--      (Just l, Just r) -> Just $ Pair l r
--  | validVal = Just $ Val $ StrVal val
--  | otherwise = Nothing
--  where
--    spl x = let
--              tl0 = dropWhile (/= x) s
--              tl1 = if tl0 == "" || tl0 == ')'
--            in
--            (takeWhile (/= x) s,  )
--    (pref, suf) = spl '('
--    exprExists = pref `elem` exprFuncs
--    sufNotEmpty = suf /= ""
--    validExpr = exprExists && sufNotEmpty
--    (pairL, pairR) = spl ','
--    validPair = pairL /= "" && pairR /= ""
--    val = if last s == ')' then init s else s
--    validVal = val /= ""

parseAst :: String -> Maybe Ast
parseAst = go ""
  where
    go prev ('(' : next) = case parseAst next of
      Nothing -> Nothing
      Just arg -> Just $ Expr prev arg
    go prev (',' : next) = parsePair prev next
    go prev (')':_) = Just $ Val $ StrVal prev 
    go prev [] = Just $ Val $ StrVal prev
    go prev (x:xs) = go (prev++[x]) xs
    parsePair l r = case (parseAst l, parseAst r) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just l, Just r) -> Just $ Pair l r

parseLevel :: String -> Maybe (Ast, String)
parseLevel = go "" (Just $ Level [])
  where
    go :: String -> Maybe Ast -> String -> Maybe (Ast, String)
    go prev (Just (Level l)) ('(':next) = case parseLevel next of
      Nothing ->
        Nothing
      Just (arg, remnant) ->
        let 
          r = if remnant /= "" && head remnant == ',' then tail remnant else remnant
        in
          go "" (Just $ Level (l ++ [Expr prev arg])) r
    go prev (Just (Level l)) (')':remnant) =
      let
        l0 = [Val $ StrVal prev | prev /= ""]
      in
        Just (Level (l ++ l0), remnant)
    go prev (Just (Level l)) (',':next) =
      let
        l0 = [Val $ StrVal prev]
      in
        go "" (Just $ Level (l ++ l0)) next
    go "" (Just l) "" = 
      Just (l, "")
    go prev (Just (Level l)) "" =
      Just (Level (l ++ [Val $ StrVal prev]), "")
    go prev l (x:xs) =
      go (prev ++ [x]) l xs

evalAst :: Ast -> Maybe Value
evalAst (Expr name ast) = 
  let
    Level args = ast
  in
  case evalLevel args of
    Nothing -> Nothing
    Just values -> getFunc name values
evalAst (Val val) = Just val
evalAst (Level level) = head $ sequence $ evalLevel level
    

evalLevel :: [Ast] -> Maybe [Value]
evalLevel = mapM evalAst


getFunc :: String -> ([Value] -> Maybe Value)
getFunc _ = eSum


-- (name, (func, minParams))
funcs0 = [("sum",(eSum, 0))]

eSum :: [Value] -> Maybe Value
eSum [] = Just $ IntVal 0
eSum (x:xs) = case coerceToInt x of
  Nothing -> Nothing
  Just a0 ->
    let
      Just (IntVal a1) = eSum xs
    in
      Just $ IntVal (a0 + a1)

