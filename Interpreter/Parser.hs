module Parser (parse) where

import Control.Applicative ()
import Control.Monad (ap, liftM)
import Data.Char (isAlphaNum, isDigit, isSpace, ord)

import Error (parserError)
import LExp

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

instance Monad Parser where
    return x = Parser (\s -> [(x, s)])
    x >>= f = Parser (\s -> let l = apply x s in concatMap aux l)
        where aux (a, s) = apply (f a) s

apply :: Parser a -> String -> [(a, String)]
apply (Parser p) = p

-- ==================================================
--
--  Parser Combinators
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
           if p c then return c else failure

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

-- ==================================================
--
--  LExp Expressions Parser
--
-- ==================================================

lname :: Parser String
lname = many1 (sat isAlphaNum)

lprimary :: Parser LExp
lprimary = parenthesized `por1` variable
        where
            parenthesized = do
                                symbol "("
                                l <- token lexpression
                                symbol ")"
                                return l
            variable = do
                        n <- token lname
                        return (LVar n)

labstraction :: Parser LExp
labstraction = do
                symbol "\\"
                n <- token lname
                symbol "->"
                l <- token lexpression
                return (LAbs n l)

latomic :: Parser LExp
latomic = lprimary `por1` labstraction

lexpression :: Parser LExp
lexpression = do l <- latomic; rest l -- left associativity
        where rest x = do { y <- latomic; rest (x :$: y) } `por1` return x

lparser :: String -> (Maybe LExp, Maybe (LExp, String))
lparser [] = parserError (LVar "") "input is empty"
lparser s = case apply lexpression s of
    [(x, [])]  -> (Just x, Nothing)
    [(x, err)] -> (Nothing, Just (x, err))
    err        -> parserError (LVar "") (show err)

-- ==================================================
--
--  Parse
--
-- ==================================================

parse :: String -> LExp
parse s = parse' (lparser s)
    where
        parse' (Just lce, Nothing)         = lce
        parse' (_, Just (unused, message)) = parserError unused message
