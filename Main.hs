import Data.Char
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List

data Expr
  = Atom String
  | Number Int
  | List [Expr]
  | Lambda Fn

instance Show Expr where
  show (Atom str) = str
  show (Number i) = show i
  show (List exprs) = "(" ++ intercalate " " (map show exprs) ++ ")"
  show (Lambda _) = "(fn)"

type Fn = Ctx -> [Expr] -> Either String (Ctx, Expr)

type Ctx = Map String Expr



------------------------



rootCtx :: Ctx
rootCtx =
  Map.fromList
    [ ("+", numFoldA (+)) 
    , ("-", numFoldA (-)) 
    , ("*", numFoldA (*)) 
    , ("/", numFoldA div) 
    , ("mod", numOpA mod) 
    , ("<", numOpB (<)) 
    , (">", numOpB (>)) 
    , ("=", numOpB (==)) 
    , ("if", Lambda ifFn)
    , ("begin", Lambda beginFn)
    , ("define", Lambda defineFn)
    , ("lambda", Lambda lambdaFn)
    , ("car", Lambda carFn)
    , ("cdr", Lambda cdrFn)
    , ("cons", Lambda consFn)
    , ("quote", Lambda quoteFn)
    , ("eval", Lambda evalFn)
    , ("let", Lambda letFn)
    ]

ifFn :: Fn
ifFn ctx [cond, a, b] =
  case eval ctx cond of
    Right (ctx', List []) -> eval ctx' b
    Right (ctx', _) -> eval ctx' a -- anything else is true
    Left e -> Left e
ifFn ctx _ =
  Left "if takes 3 arguments"

numFoldA :: (Int -> Int -> Int) -> Expr
numFoldA op = Lambda $
    \ctx args ->
      case evalAll ctx args of
        Right (ctx', argVs) ->
            case allNumbers argVs of
                Just ns ->
                    Right (ctx', Number (foldl1 op ns))
                Nothing ->
                    Left $ "all arguments should be numbers"
        Left e ->
            Left e
  where allNumbers xs = if all isNumber xs
                          then Just $ map (\(Number n) -> n) xs
                          else Nothing
        isNumber (Number _) = True
        isNumber _ = False

numOpA :: (Int -> Int -> Int) -> Expr
numOpA op = Lambda $
    \ctx [a, b] ->
      case eval ctx a of
        Right (ctx', Number x) ->
          case eval ctx' b of
            Right (ctx'', Number y) -> Right (ctx'', Number (op x y))
            Right (_, b') -> Left $ "tried arith on non number '" ++ show b' ++ "'"
        Right (_, a') -> Left $ "tried arith on non number '" ++ show a' ++ "'"

numOpB :: (Int -> Int -> Bool) -> Expr
numOpB op = Lambda $
    \ctx [a, b] ->
      case eval ctx a of
        Right (ctx', Number x) ->
          case eval ctx' b of
            Right (ctx'', Number y) ->
                if op x y
                    then Right (ctx'', Atom "t")
                    else Right (ctx'', List [])
            Right (_, b') -> Left $ "tried boolean on non number '" ++ show b' ++ "'"
        Right (_, a') -> Left $ "tried boolean on non number '" ++ show a' ++ "'"

beginFn :: Fn
beginFn ctx [] = Left "empty begin"
beginFn ctx [e] = eval ctx e
beginFn ctx (e:es) =
  case eval ctx e of
    Right (ctx', _) -> beginFn ctx' es
  
lambdaFn :: Fn
lambdaFn defCtx ((List args):body) =
  Right $
    ( defCtx
    , Lambda $
      \callCtx params ->
        let
          Right (callCtx', params') = evalAll callCtx params
          argValues = Map.fromList $ zipWith (\(Atom a) p -> (a, p)) args params'
          ctx = argValues `Map.union` callCtx' `Map.union` defCtx
        in
          beginFn ctx body
    )

defineFn :: Fn
defineFn ctx [(Atom key), t] =
  let
    Right (ctx', v) = eval ctx t
  in
    Right (Map.insert key v ctx', v)
defineFn ctx _ =
  Left "invalid variable name"

carFn :: Fn
carFn ctx [e] =
  case eval ctx e of
    Right (ctx', List (x:_)) ->
      Right (ctx', x)
    _ ->
      Left "car of non list"
carFn ctx _ =
  Left "car takes one argument"

cdrFn :: Fn
cdrFn ctx [e] =
  case eval ctx e of
    Right (ctx', List (_:xs)) ->
      Right (ctx', List xs)
    _ ->
      Left "cdr of non list"
cdrFn ctx _ =
  Left "cdr takes one argument"

consFn :: Fn
consFn ctx [head, tail] =
  case eval ctx head of
    Right (ctx', vHead) ->
      case eval ctx' tail of
        Right (ctx'', List vTail) ->
          Right (ctx'', List (vHead:vTail))
        _ ->
          Left "cons to non list"
consFn ctx _ =
  Left "cons takes two arguments"

quoteFn :: Fn
quoteFn ctx [x] = Right (ctx, x)
quoteFn ctx _ =
  Left "quote takes one argument"

evalFn :: Fn
evalFn ctx [e] =
  let
    Right (ctx', e') = eval ctx e
  in
    eval ctx' e'
evalFn ctx _ =
  Left "eval takes one argument"

letFn :: Fn
letFn ctx (List bindings:body) =
    if all isBinding bindings then
      let
        bs = map (\(List [Atom a, exp]) -> (a, exp)) bindings
      in
        beginFn (Map.union (Map.fromList bs) ctx) body
    else
      Left "first argument to let should be a list of bindings"
  where isBinding (List [Atom _, _]) = True
        isBinding _ = False
letFn ctx _ =
  Left "first argument to let should be a list"


-----------------------



main :: IO ()
main = 
  do
    core <- readFile "core.lisp"
    let coreExprs = map fst $ mapMaybe parse $ lines core
    let Right (coreCtx, _) = evalAll rootCtx coreExprs
    repl coreCtx

repl :: Ctx -> IO ()
repl ctx =
  do
    putStr "lisp> "
    inp <- getLine
    case inp of
      ":q" -> return ()
      ":ctx" -> putStrLn (show ctx) >> repl ctx
      _ ->
        case parse inp of
          Just (e, "") ->
            case eval ctx e of
              Right (ctx', e) -> 
                do putStrLn (show e)
                   repl (Map.insert "_" e ctx')
              Left e -> 
                do putStrLn $ "ERROR: " ++ e
                   repl ctx
          _ ->
            do putStrLn "ERROR: no parse!"
               repl ctx



--------------------



parse :: String -> Maybe (Expr, String)
parse str =
  case number str of
    Just a -> Just a
    Nothing ->
        case atom str of
            Just a -> Just a
            Nothing -> list str
    
number :: String -> Maybe (Expr, String)
number str =
  case span isDigit str of
    ("", _) -> Nothing
    (n, more) ->
        Just (Number (read n), more)
    
atom :: String -> Maybe (Expr, String)
atom str =
  case span (\c -> c /= '(' && c /= ' ' && c /= ')') str of
    ("", _) -> Nothing
    (atom, more) ->
        Just (Atom atom, more)

list :: String -> Maybe (Expr, String)
list ('(':str) =
  case parseMany str of
    Just (some, ')':moreStr) -> Just (List some, moreStr)
    _ -> Nothing
list _ = Nothing

parseMany :: String -> Maybe ([Expr], String)
parseMany str =
    case parse str of
        Just (x, more) ->
          case more of
            ' ':afterSpace ->
              let
                Just (xs, after) = parseMany afterSpace
              in
                Just (x:xs, after)
            _ ->
              Just ([x], more)
        Nothing ->
            Just ([], str)



--------------------------------------



eval :: Ctx -> Expr -> Either String (Ctx, Expr)

eval ctx (Atom "t") = Right (ctx, Atom "t")
eval ctx (Atom s) =
    case Map.lookup s ctx of
      Just e -> Right (ctx, e)
      Nothing -> Left "atom not defined"

eval ctx (Number i) = Right (ctx, Number i)

eval ctx l@(Lambda _) = Right (ctx, l)

eval ctx (List []) = Right (ctx, List [])

eval ctx (List ((Atom fnName):args)) =
    case Map.lookup fnName ctx of
      Just (Lambda fn) ->
        fn ctx args
      _ ->
        Left $ "function '" ++ fnName ++ "' not defined"

eval ctx (List ((Lambda fn):args)) = fn ctx args

eval ctx x = Left $ "idk how to eval '" ++ show x ++ "'"


evalAll :: Ctx -> [Expr] -> Either String (Ctx, [Expr])
evalAll ctx [] = Right (ctx, [])
evalAll ctx (e:es) =
  case eval ctx e of
    Right (ctx', v) -> 
      let Right (final, vs) = evalAll ctx' es
      in Right (final, v:vs)

