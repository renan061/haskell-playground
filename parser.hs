import Control.Applicative ()
import Control.Monad (ap, liftM)
import Data.Char (isAlphaNum, isDigit, isSpace, ord)

-- ==================================================
--
--  Lambda Type
--
-- ==================================================

type Name = String

data Lambda = LVar Name
                      | LApp Lambda Lambda
                      | LAbs Name Lambda

instance Show Lambda where
  show (LVar n) = n
  show (LApp l1 l2) = "(" ++ (show l1) ++ " " ++ (show l2) ++ ")"
  show (LAbs n l) = "(\\" ++ n ++ " -> " ++ (show l) ++ ")"

-- ==================================================
--
--  Parser Monad
--
-- ==================================================

newtype Parser a = Parser (String -> [(a, String)])

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

-- apply
parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

instance Monad Parser where
    return x = Parser (\s -> [(x, s)])
    x >>= f = Parser (\s -> let l = parse x s in concatMap aux l)
        where aux (a, s) = parse (f a) s

-- ==================================================
--
--  Parsers
--
-- ==================================================

failure :: Parser a
failure = Parser (\_ -> [])

item :: Parser Char
item = Parser aux
    where aux [] = []
          aux (x:xs) = [(x, xs)]

-- deterministic parser or
por1 :: Parser a -> Parser a -> Parser a
por1 (Parser f) (Parser g) = Parser aux
    where aux s = case (f s) of [] -> g s
                                l  -> l

-- satisfies
sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           if p c then return c
                  else failure

digit :: Parser Int
digit = do c <- sat isDigit
           return (ord c - ord '0')

number :: Parser Int
number = aux 0
    where aux n = do c <- digit
                     let val = n * 10 + c in aux val `por1` return val

-- *
many :: Parser a -> Parser [a]
many p = many1 p `por1` return []

-- +
many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser ()
string [] = return ()
string (x:xs) = do char x
                   string xs

token :: Parser a -> Parser a
token t = do x <- t
             spaces
             return x

symbol :: String -> Parser ()
symbol = token . string

space :: Parser Char
space = sat isSpace

comment :: Parser String
comment = do char '#'
             many (sat (/= '\n'))
             many space

spaces :: Parser String
spaces = do many space
            comment `por1` many space

-- TODO: Not compiling
-- pnot :: Parser a -> Parser ()
-- pnot p = Parser (\s -> if p s == [] then [((), s)] else [])

-- string [] = pnot (sat isAlphaNum)

-- ==================================================
--
--  Lambda Expressions Parser
--
-- ==================================================

lname :: Parser String
lname = many1 (sat isAlphaNum)

lprimary :: Parser Lambda
lprimary = parenthesized `por1` variable
    where parenthesized = do symbol "("
                             l <- token lexpression
                             symbol ")"
                             return l
          variable = do n <- token lname
                        return (LVar n)

labstraction :: Parser Lambda
labstraction = do symbol "/"
                  n <- token lname
                  symbol "->"
                  l <- token lexpression
                  return (LAbs n l)

latomic :: Parser Lambda
latomic = lprimary `por1` labstraction

lexpression :: Parser Lambda
lexpression = do l <- latomic; rest l -- left associativity
    where rest x = do y <- latomic
                      rest (LApp x y)
                   `por1` return x

lparser :: String -> (Maybe Lambda, Maybe (Lambda, String))
lparser [] = error "error: input is empty"
lparser s = case parse lexpression s of [(x, [])]  -> (Just x, Nothing)
                                        [(x, err)] -> (Nothing, Just (x, err))
                                        _          -> error "error..."

-- ==================================================
--
--  Main
--
-- ==================================================

lprint :: (Maybe Lambda, Maybe (Lambda, String)) -> IO ()
lprint (Just x, Nothing) = print x 
lprint (Nothing, Just (x, err)) = putStrLn e
    where e = "error:\n\twrong: " ++ (show x) ++ "\n\tunused: " ++ (show err)
lprint _ = error "error: internal"

main = do lprint (lparser "x    ")
          lprint (lparser "(x)")
          lprint (lparser "/x  ->  (x)  ")
          lprint (lparser "(/x -> x) y")
          lprint (lparser "x y")
          lprint (lparser "(/x -> x y) x")
          lprint (lparser "(/x -> /y -> x y) a b")
          lprint (lparser "(a))") -- unmatched parentheses
          lprint (lparser "x #comentário")
          lprint (lparser "(/x -> /y -> x y) a b # mais comentário")
          lprint (lparser "(/x -> /y -> x y) a b # mais # comentário dentro")





