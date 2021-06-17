import Data.List
import Data.Char
import Debug.Trace

data Result
    = Ok Ast
    | ParseError
    | EvalError

main :: IO ()
main = do line <- readFile "core.lisp"
          let (env, msg) = exe [] line
          if env == [] then
            putStrLn msg >> loop []
          else
            loop env

loop :: Env -> IO ()
loop env =
    do putStr "lisp> "
       line <- getLine
       if line == ":dump" then
           do putStrLn (show env)
              loop env
       else if line == ":exit" then
           return ()
       else
           do let (env', msg) = exe env line
              putStrLn msg
              loop env'

exe :: Env -> String -> (Env, String)
exe env code = 
    case (parse . tokenise) code of
        Just ast ->
            case eval env ast of
                Right (env', out) ->
                    ( env'
                    , "=> " ++ show out
                    )
                Left e ->
                    ( env
                    , case e of
                        Unbound name ->
                            "unbound symbol '" ++ name ++ "'"
                        UndefinedFunction code ->
                            "function " ++ code ++ " not defined"
                        Unimplemented code ->
                            "I can't eval " ++ code
                        WrongArgNumber need given ->
                            "Wrong number of args: " ++
                            (show need) ++ " needed, " ++ (show given) ++
                            " provided"
                    )
        Nothing ->
                ( env
                , "parse error"
                )

-----------------{{{

data Token
    = Open
    | Close
    | Dot
    | Quote
    | Other String

instance Show Token where
    show Open = "("
    show Close = ")"
    show Dot = "."
    show Quote = "'"
    show (Other x) = show x

tokenise :: String -> [Token]
tokenise code = map toToken $ words san
    where san = replace "(" " ( " $
                replace ")" " ) " $
                replace "\n" "" $
                replace "'" " ' " $
                replace "." " . " code
    
replace :: String -> String -> String -> String
replace find repl "" = ""
replace find repl str =
    if find `isPrefixOf` str
        then repl ++ (replace find repl (drop (length find) str))
        else (head str) : replace find repl (tail str)

toToken :: String -> Token
toToken "(" = Open
toToken "." = Dot
toToken ")" = Close
toToken "'" = Quote
toToken  x  = Other x

-----------------}}}

data Ast
    = Atom String
    | Number Int
    | Cons Ast Ast
    | Nil
    | Lambda Ast Ast
    | T deriving (Eq)--, Show)

instance Show Ast where
    show (Atom str) = str
    show (Number n) = show n
    show Nil = "()"
    show T = "t"
    --show (Lambda _ _) = "#lambda"
    show (Lambda args body) = "(\\ " ++ show args ++ " " ++ show body ++ ")"
    show list@(Cons a b) =
        "(" ++ ( case well_formed list of
                   Just contents -> intercalate " " $ map show contents
                   Nothing -> show a ++ " . " ++ show b
        ) ++ ")"

-----------------{{{

parse :: [Token] -> Maybe Ast
parse tokens =
    case root tokens of
        Just (ast, []) -> Just ast
        _ -> Nothing

root :: [Token] -> Maybe (Ast, [Token])
root ts =
    case quote ts of
        Just x -> Just x
        Nothing ->
            case list ts of
                Just x -> Just x
                Nothing ->
                    case cons ts of
                        Just x -> Just x
                        Nothing ->
                            non_cons ts

quote (Quote:more) =
    case root more of
        Just (ast, even_more) ->
            Just (Cons (Atom "quote") (Cons ast Nil), even_more)
        Nothing ->
            Nothing
quote _ = Nothing

list (Open:more) = list_end more
list _ = Nothing

list_end (Close:more) = Just (Nil, more)
list_end [] = Nothing
list_end more =
    case root more of
        Nothing -> Nothing
        Just (ast1, even_more) ->
            case list_end even_more of
                Nothing -> Nothing
                Just (ast2, even_even_more) ->
                    Just (Cons ast1 ast2, even_even_more)

cons (Open:more) =
    case root more of
        Just (ast1, Dot:even_more) ->
            case root even_more of
                Just (ast2, Close:even_even_more) ->
                    Just (Cons ast1 ast2, even_even_more)
                _ -> Nothing
        _ -> Nothing
cons _ = Nothing

non_cons ts =
    case ts of
        Other s : more ->
            Just
                ( if all isNumber s then Number (read s)
                  else if s == "t" then T
                  else Atom s
                , more
                )
        _ ->
            Nothing

-----------------}}}

data EvalError
  = Unbound String
  | Unimplemented String
  | UndefinedFunction String
  | WrongArgNumber Int Int

type Env = [(String, Ast)]

eval :: Env -> Ast -> Either EvalError (Env, Ast)

eval env (Number int) = Right (env, Number int)

eval env Nil = Right (env, Nil)

eval env T = Right (env, T)

eval env (Atom name) =
    case lookup name built_in of
        Just val -> Right (env, (Atom name))
        Nothing ->
            case lookup name env of
                Just val -> Right (env, val)
                Nothing -> Left (Unbound name)

eval env l@(Lambda _ _) = Right (env, l)

eval env (Cons (Lambda args body) tail) =
    let
        Just args' = well_formed args
        Just tail' = well_formed tail
    in
        if length args' == length tail' then
            eval env (substitute args' tail' body)
        else
            Left (WrongArgNumber (length args') (length tail'))

eval env (Cons head@(Cons _ _) tail) =
    do (env', head') <- eval env head 
       eval env' (Cons head' tail)

eval env (Cons (Atom fn_name) tail) =
    case lookup fn_name built_in of
        Just fn ->
            fn env tail
        Nothing ->
            case lookup fn_name env of
                Just x -> eval env (Cons x tail)
                Nothing -> Left (UndefinedFunction fn_name)

eval env z = Left (Unimplemented (show z))

-----------

eval_all :: Env -> [Ast] -> Either EvalError (Env, [Ast])
eval_all env [] = Right (env, [])
eval_all env (v:vs) =
    do (env', v') <- eval env v
       (env'', vs') <- eval_all env' vs
       return (env'', v':vs')

substitute :: [Ast] -> [Ast] -> Ast -> Ast
substitute args tail body = sub (zip names tail) body
    where names = map (\(Atom s) -> s) args

sub :: [(String, Ast)] -> Ast -> Ast
sub dict (Atom s) =
    case lookup s dict of
        Just v -> v
        Nothing -> Atom s
sub _ (Number n) = Number n
sub dict (Cons (Atom "quote") ast) =
    Cons (Atom "quote") ast
sub dict (Cons (Atom "fn") (Cons args body)) =
    Cons (Atom "fn") (Cons args (sub dict body))
sub dict (Cons ast1 ast2) =
    Cons (sub dict ast1) (sub dict ast2)
sub dict (Lambda args body) =
    -- TODO don't sub if dict & args intersect
    Lambda args (sub dict body)
sub _ Nil = Nil
sub _ T = T

well_formed :: Ast -> Maybe [Ast]
well_formed Nil = Just []
well_formed (Cons a b) = well_formed b >>= \x -> Just (a : x)
well_formed _ = Nothing

to_cons :: [Ast] -> Ast
to_cons [] = Nil
to_cons (x:xs) = Cons x (to_cons xs)

-----------------{{{

built_in =
    [ ("+", _add)
    , ("-", _sub)
    , ("*", _mul)
    , ("/", _div)
    , ("mod", _mod)
    , ("=", _eq)
    , ("<", _lt)
    , ("eval", _eval)
    , ("if", _if)
    , ("cond", _cond)
    , ("let", _let)
    , ("quote", _quote)
    , ("car", _car)
    , ("cdr", _cdr)
    , ("cons", _cons)
    , ("list", _list)
    , ("begin", _begin)
    , ("def", _def)
    , ("fn", _fn)
    , ("apply", _apply)
    ]

arith fn env tail =
    do let Just vs = well_formed tail
       (env', vs') <- eval_all env vs
       let nums = map (\(Number n) -> n) vs'
       return (env', Number (foldl1 fn nums))

_add = arith (+)

_sub = arith (-)

_mul = arith (*)

_div = arith div

_mod = arith mod

_eq env tail =
    do let Cons a (Cons b Nil) = tail
       (env' , Number x) <- eval env a
       (env'', Number y) <- eval env' b
       return (env'', if x == y then T else Nil)

_lt env tail =
    do let Cons a (Cons b Nil) = tail
       (env' , Number x) <- eval env a
       (env'', Number y) <- eval env' b
       return (env'', if x < y then T else Nil)

_eval env tail = eval env exp
    where (Cons exp Nil) = tail

_if env tail =
    do let Cons cond (Cons true (Cons false Nil)) = tail
       (env', cond') <- eval env cond
       if cond' == Nil then eval env' false
                       else eval env' true

_cond env tail =
    case tail of
        Nil -> Right (env, Nil)
        (Cons (Cons cond (Cons ret Nil)) more) ->
            case eval env cond of
                Right (env', cond') ->
                    case cond' of
                        Nil -> _cond env' more
                        _ -> eval env' ret
                Left x ->
                    Left x

_let env tail =
    let
        Cons assigns (Cons body Nil) = tail
        Just assigns' = well_formed assigns
        pairs = map (\(Cons (Atom name) (Cons v Nil)) -> (name, v)) assigns'
    in
        eval (pairs ++ env) body

_quote env tail = Right (env, inner)
    where Cons inner Nil = tail

_car env args =
    do let Cons arg Nil = args
       (env', arg') <- eval env arg
       let Cons head _ = arg'
       return (env', head)

_cdr env args =
    do let Cons arg Nil = args
       (env', arg') <- eval env arg
       let Cons _ tail = arg'
       return (env', tail)

_cons env tail =
    do let Cons a (Cons b Nil) = tail
       (env' , a') <- eval env a
       (env'', b') <- eval env' b
       return (env'', Cons a' b')

_list env tail =
    do let Just all = well_formed tail
       (env', evald) <- eval_all env all
       return (env', to_cons evald)

_begin env tail =
    do let Just all = well_formed tail
       (env', evald) <- eval_all env all
       return (env', last evald)

_def env tail =
    do let Cons (Atom name) (Cons v Nil) = tail
       (env', v') <- eval env v
       return ((name, v') : env', Nil)

_fn env tail = Right (env, closure)
    where Cons args (Cons body Nil) = tail
          closure = Lambda args body

_apply env tail =
    do let Cons fn_name (Cons args Nil) = tail
       (env', args') <- eval env args
       eval env' (Cons fn_name args')

----------------}}}
