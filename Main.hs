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
          loop env

loop :: Env -> IO ()
loop env =
    do putStr "lisp> "
       line <- getLine
       let (env', msg) = exe env line
       putStrLn msg
       loop env'

exe :: Env -> String -> (Env, String)
exe env code = 
    case (parse . tokenise) code of
        Just ast ->
            case eval env ast of
                (env', out) ->
                    (env', "=> " ++ show out)
        Nothing ->
                (env, "parse error")

-----------------{{{

data Token
    = Open
    | Close
    | Dot
    | Other String

instance Show Token where
    show Open = "("
    show Close = ")"
    show Dot = "."
    show (Other x) = show x

tokenise :: String -> [Token]
tokenise code = map toToken $ words san
    where san = replace "(" " ( " $
                replace ")" " ) " $
                replace "\n" "" $
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
    show (Lambda args body) = "(#lambda)"
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
    case list ts of
        Just x -> Just x
        Nothing ->
            case cons ts of
                Just x -> Just x
                Nothing ->
                    non_cons ts

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

type Env = [(String, Ast)]

eval :: Env -> Ast -> (Env, Ast)

eval env (Number int) = (env, Number int)

eval env Nil = (env, Nil)

eval env T = trace "!" (env, T)

eval env (Atom string) =
    case lookup string env of
        Just val -> (env, val)
        Nothing -> error ("undefined " ++ string)

eval env l@(Lambda _ _) = (env, l)

eval env (Cons (Lambda args body) tail) =
    let
        Just args' = well_formed args
        Just tail' = well_formed tail
    in
        eval env (substitute args' tail' body)

eval env (Cons head@(Cons _ _) tail) = eval env' (Cons head' tail)
    where (env', head') = eval env head 

--------------

eval env (Cons (Atom "+") tail) = (env', Number (foldl1 (+) nums))
    where Just vs = well_formed tail
          (env', vs') = eval_all env vs
          nums = map (\(Number n) -> n) vs'

eval env (Cons (Atom "-") tail) = (env', Number (foldl1 (-) nums))
    where Just vs = well_formed tail
          (env', vs') = eval_all env vs
          nums = map (\(Number n) -> n) vs'

eval env (Cons (Atom "*") tail) = (env', Number (foldl1 (*) nums))
    where Just vs = well_formed tail
          (env', vs') = eval_all env vs
          nums = map (\(Number n) -> n) vs'

eval env (Cons (Atom "/") tail) = (env', Number (foldl1 div nums))
    where Just vs = well_formed tail
          (env', vs') = eval_all env vs
          nums = map (\(Number n) -> n) vs'

eval env (Cons (Atom "mod") tail) = (env', Number (foldl1 mod nums))
    where Just vs = well_formed tail
          (env', vs') = eval_all env vs
          nums = map (\(Number n) -> n) vs'

eval env (Cons (Atom "=") tail) = (env'', if x == y then T else Nil)
    where Cons a (Cons b Nil) = tail
          (env' , Number x) = eval env a
          (env'', Number y) = eval env' b

eval env (Cons (Atom "if") tail) =
    if cond' == Nil then eval env' false
                    else eval env' true
    where Cons cond (Cons true (Cons false Nil)) = tail
          (env', cond') = eval env cond

eval env (Cons (Atom "quote") tail) = (env, inner)
    where Cons inner Nil = tail

eval env (Cons (Atom "car") args) = (env', head)
    where Cons arg Nil = args
          (env', arg') = eval env arg
          Cons head _ = arg'

eval env (Cons (Atom "cdr") args) = (env', tail)
    where Cons arg Nil = args
          (env', arg') = eval env arg
          Cons _ tail = arg'

eval env (Cons (Atom "cons") tail) = (env'', Cons a' b')
    where Cons a (Cons b Nil) = tail
          (env' , a') = eval env a
          (env'', b') = eval env' b

eval env (Cons (Atom "list") tail) = (env', to_cons evald)
    where Just all = well_formed tail
          (env', evald) = eval_all env all

eval env (Cons (Atom "begin") tail) = (env', last evald)
    where Just all = well_formed tail
          (env', evald) = eval_all env all

eval env (Cons (Atom "def") tail) = ((name, v') : env', Nil)
    where Cons (Atom name) (Cons v Nil) = tail
          (env', v') = eval env v

eval env (Cons (Atom "fn") tail) = (env, closure)
    where Cons args (Cons body Nil) = tail
          closure = Lambda args body

eval env (Cons (Atom fn_name) tail) =
    case lookup fn_name env of
        Just x -> eval env (Cons x tail)
        Nothing -> error ("function " ++ fn_name ++ " undefined")

-----------

eval env z = error $ "nomatch " ++ show z

-----------

eval_all :: Env -> [Ast] -> (Env, [Ast])
eval_all env [] = (env, [])
eval_all env (v:vs) = (env'', v':vs')
    where (env', v') = eval env v
          (env'', vs') = eval_all env' vs

substitute :: [Ast] -> [Ast] -> Ast -> Ast
substitute args tail body = sub (zip names tail) body
    where names = map (\(Atom s) -> s) args

sub :: [(String, Ast)] -> Ast -> Ast
sub dict (Atom s) =
    case lookup s dict of
        Just v -> v
        Nothing -> Atom s
sub _ (Number n) = Number n
sub dict (Cons ast1 ast2) =
    Cons (sub dict ast1) (sub dict ast2)
sub dict (Lambda args body) = error "TODO"
sub _ Nil = Nil
sub _ T = T

well_formed :: Ast -> Maybe [Ast]
well_formed Nil = Just []
well_formed (Cons a b) = well_formed b >>= \x -> Just (a : x)
well_formed _ = Nothing

to_cons :: [Ast] -> Ast
to_cons [] = Nil
to_cons (x:xs) = Cons x (to_cons xs)
