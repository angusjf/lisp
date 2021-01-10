import Data.Char

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

type Ctx = [(String, Expr)]



------------------------



rootCtx :: Ctx
rootCtx =
  [ ("+", numOp (+)) 
  , ("-", numOp (-)) 
  , ("*", numOp (*)) 
  , ("/", numOp div) 
  , ("if", Lambda ifFn)
  , ("begin", Lambda beginFn)
  , ("define", Lambda defineFn)
  , ("lambda", Lambda lambdaFn)
  ]

ifFn :: Fn
ifFn ctx [cond, a, b] =
  case eval ctx cond of
    (ctx', Atom "#t") -> eval ctx' a
    (ctx', Atom "#f") -> eval ctx' b
    _ -> error "bad if statement"

numOp :: (Int -> Int -> Int) -> Expr
numOp op = Lambda $
    \ctx [a, b] ->
      case eval ctx a of
        (ctx', Number x) ->
          case eval ctx' b of
            (ctx'', Number y) -> (ctx'', Number (op x y))
            (_, b') -> error $ "tried arith on non number '" ++ show b' ++ "'"
        (_, a') -> error $ "tried arith on non number '" ++ show a' ++ "'"

beginFn :: Fn
beginFn ctx [] = error "empty begin"
beginFn ctx [e] = eval ctx e
beginFn ctx (e:es) =
  case eval ctx e of
    (ctx', _) -> beginFn ctx' es
  

evalAll :: Ctx -> [Expr] -> (Ctx, [Expr])
evalAll ctx [] = (ctx, [])
evalAll ctx (e:es) =
  case eval ctx e of
    (ctx', v) -> 
      let (final, vs) = evalAll ctx' es
      in (final, v:vs)

lambdaFn :: Fn
lambdaFn defCtx ((List args):body) =
  ( defCtx
  , Lambda $
    \callCtx params ->
      let
        (callCtx', params') = evalAll callCtx params
        argValues = zipWith (\(Atom a) p -> (a, p)) args params'
        ctx = callCtx' ++ defCtx ++ argValues
      in
        beginFn ctx body
  )

defineFn :: Fn
defineFn ctx [(Atom key), t] =
  let
    (ctx', v) = eval ctx t
  in
    (((key, v):ctx'), v)
defineFn ctx _ = error "invalid variable name"

main :: IO ()
main = repl rootCtx

repl :: Ctx -> IO ()
repl ctx =
  do
    putStr ">>> "
    inp <- getLine
    case parse inp of
      Just (e, "") ->
        case eval ctx e of
          (ctx', e) -> 
            do putStrLn (show e)
               repl ctx'
      Nothing ->
        do putStrLn "ERROR: no parse!"
           repl ctx

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
  case span (\c -> c /= '(' && c /= ' ') str of
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

eval ctx (Atom s) =
    case lookup s ctx of
      Just e -> (ctx, e)
      Nothing -> error "atom not defined"

eval ctx (Number i) = (ctx, Number i)

eval ctx l@(Lambda _) = (ctx, l)

eval ctx (List []) = (ctx, List [])

eval ctx (List ((Atom fnName):args)) =
    case lookup fnName ctx of
      Just (Lambda fn) ->
        fn ctx args
      _ ->
        error $ "fn '" ++ fnName ++ "' not defined"

eval ctx (List ((Lambda fn):args)) =
  fn ctx args

eval ctx x = error $ "idk how to eval '" ++ show x ++ "'"
