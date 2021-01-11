import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Expr
  = Atom String
  | Number Int
  | List [Expr]
  | Lambda Fn

instance Show Expr where
  show (Atom str) = str
  show (Number i) = show i
  show (List exprs) = show exprs
  show (Lambda _) = "(fn)"

type Fn = Ctx -> [Expr] -> (Ctx, Expr)

type Ctx = Map String Expr



------------------------



rootCtx :: Ctx
rootCtx =
  Map.fromList
    [ ("+", numOpA (+)) 
    , ("-", numOpA (-)) 
    , ("*", numOpA (*)) 
    , ("/", numOpA div) 
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
    ]

ifFn :: Fn
ifFn ctx [cond, a, b] =
  case eval ctx cond of
    (ctx', List []) -> eval ctx' b
    (ctx', _) -> eval ctx' a -- anything else is true
ifFn ctx _ =
  error "if takes 3 arguments"

numOpA :: (Int -> Int -> Int) -> Expr
numOpA op = Lambda $
    \ctx [a, b] ->
      case eval ctx a of
        (ctx', Number x) ->
          case eval ctx' b of
            (ctx'', Number y) -> (ctx'', Number (op x y))
            (_, b') -> error $ "tried arith on non number '" ++ show b' ++ "'"
        (_, a') -> error $ "tried arith on non number '" ++ show a' ++ "'"

numOpB :: (Int -> Int -> Bool) -> Expr
numOpB op = Lambda $
    \ctx [a, b] ->
      case eval ctx a of
        (ctx', Number x) ->
          case eval ctx' b of
            (ctx'', Number y) ->
                if op x y
                    then (ctx'', Atom "t")
                    else (ctx'', List [])
            (_, b') -> error $ "tried boolean on non number '" ++ show b' ++ "'"
        (_, a') -> error $ "tried boolean on non number '" ++ show a' ++ "'"

beginFn :: Fn
beginFn ctx [] = error "empty begin"
beginFn ctx [e] = eval ctx e
beginFn ctx (e:es) =
  case eval ctx e of
    (ctx', _) -> beginFn ctx' es
  
lambdaFn :: Fn
lambdaFn defCtx ((List args):body) =
  ( defCtx
  , Lambda $
    \callCtx params ->
      let
        (callCtx', params') = evalAll callCtx params
        argValues = Map.fromList $ zipWith (\(Atom a) p -> (a, p)) args params'
        ctx = argValues `Map.union` callCtx' `Map.union` defCtx
      in
        beginFn ctx body
  )

defineFn :: Fn
defineFn ctx [(Atom key), t] =
  let
    (ctx', v) = eval ctx t
  in
    (Map.insert key v ctx', v)
defineFn ctx _ =
  error "invalid variable name"

carFn :: Fn
carFn ctx [e] =
  case eval ctx e of
    (ctx', List (x:_)) ->
      (ctx', x)
    _ ->
      error "car of non list"
carFn ctx _ =
  error "car takes one argument"

cdrFn :: Fn
cdrFn ctx [e] =
  case eval ctx e of
    (ctx', List (_:xs)) ->
      (ctx', List xs)
    _ ->
      error "cdr of non list"
cdrFn ctx _ =
  error "cdr takes one argument"

consFn :: Fn
consFn ctx [head, tail] =
  case eval ctx head of
    (ctx', vHead) ->
      case eval ctx' tail of
        (ctx'', List vTail) ->
          (ctx'', List (vHead:vTail))
        _ ->
          error "cons to non list"
consFn ctx _ =
  error "cons takes two arguments"

quoteFn :: Fn
quoteFn ctx [x] = (ctx, x)



-----------------------



main :: IO ()
main = 
  do
    core <- readFile "core.al"
    let coreExprs = map fst $ mapMaybe parse $ lines core
    let coreCtx = fst $ evalAll rootCtx coreExprs
    repl coreCtx

repl :: Ctx -> IO ()
repl ctx =
  do
    putStr "lisp> "
    inp <- getLine
    case parse inp of
      Just (e, "") ->
        case eval ctx e of
          (ctx', e) -> 
            do putStrLn $ (show e)
               repl (Map.insert "_" e ctx')
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



eval :: Ctx -> Expr -> (Ctx, Expr)

eval ctx (Atom "t") = (ctx, Atom "t")
eval ctx (Atom s) =
    case Map.lookup s ctx of
      Just e -> (ctx, e)
      Nothing -> error "atom not defined"

eval ctx (Number i) = (ctx, Number i)

eval ctx l@(Lambda _) = (ctx, l)

eval ctx (List []) = (ctx, List [])

eval ctx (List ((Atom fnName):args)) =
    case Map.lookup fnName ctx of
      Just (Lambda fn) ->
        fn ctx args
      _ ->
        error $ "function '" ++ fnName ++ "' not defined"

eval ctx (List ((Lambda fn):args)) = fn ctx args

eval ctx x = error $ "idk how to eval '" ++ show x ++ "'"


evalAll :: Ctx -> [Expr] -> (Ctx, [Expr])
evalAll ctx [] = (ctx, [])
evalAll ctx (e:es) =
  case eval ctx e of
    (ctx', v) -> 
      let (final, vs) = evalAll ctx' es
      in (final, v:vs)

