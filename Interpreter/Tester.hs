import LExp
import Parser
import Translator
import Evaluator

main = do
    putStrLn "Parser:\n-------"
    testParser
    putStrLn "Translator:\n------------"
    testTranslator
    putStrLn "Interpreter:\n------------"
    testInterpreter

-- ==================================================
--
--  Parser
--
-- ==================================================

testParser :: IO ()
testParser = putStrLn (if result == "" then "Ok" else result)
    where
        result = compare' (show . parse) show cases
        cases = [
            (
                "x    ",
                LVar "x"
            ),
            (
                "(x)",
                LVar "x"
            ),
            (
                "\\x  ->  (x)  ",
                LAbs "x" (LVar "x")
            ),
            (
                "(\\x -> x) y",
                LAbs "x" (LVar "x") :$: LVar "y"
            ),
            (
                "x y",
                LVar "x" :$: LVar "y"
            ),
            (
                "(\\x -> x y) x",
                LAbs "x" (LVar "x" :$: LVar "y") :$: LVar "x"
            ),
            (
                "(\\x -> \\y -> y x) a b",
                (LAbs "x" (LAbs "y" (LVar "y" :$: LVar "x"))) :$: LVar "a" :$: LVar "b"
            ),
            (
                "x (y z)",
                LVar "x" :$: (LVar "y" :$: (LVar "z"))
            ),  
            (
                "x y z",
                LVar "x" :$: (LVar "y") :$: (LVar "z")
            ),
            (
                "(\\n -> n) 10",
                (LAbs "n" (LVar "n")) :$: (LVar "10")
            ),
            (
                "xyz10 abc11",
                LVar "xyz10" :$: (LVar "abc11")
            ),
            (
                "x #comentário",
                LVar "x"
            ),  
            (
                "(\\x -> \\y -> x y) a b # mais comentário",
                (LAbs "x" (LAbs "y" (LVar "x" :$: LVar "y"))) :$: LVar "a" :$: LVar "b"
            )]

-- ==================================================
--
--  Translator
--
-- ==================================================

testTranslator :: IO ()
testTranslator = putStrLn (if result == "" then "Ok" else result)
    where
        result = compare' (show . translate) id cases
        cases = [
            (
                LAbs "x" (LVar "x"),
                "I"
            ),
            (
                LAbs "x" (LVar "y"),
                "(K y)"
            ),
            (
                LAbs "x" (LAbs "x" (LVar "y")),
                "(K (K y))"
            ),
            (
                LAbs "x" (LAbs "x" (LVar "x")),
                "(K I)"
            ),
            (
                LAbs "x" (LAbs "y" (LVar "x")),
                "K"
            ),
            (
                LAbs "x" (LAbs "y" (LVar "y")),
                "(K I)"
            ),
            (
                -- \x -> x x
                LAbs "x" (LVar "x" :$: LVar "x"),
                "((S I) I)"
            ),
            (
                -- \x -> x y
                LAbs "x" ((LVar "x") :$: (LVar "y")),
                "((C I) y)"
            ),
            (
                -- \y -> f x y
                LAbs "y" ((LVar "f") :$: (LVar "x") :$: (LVar "y")),
                "(f x)"
            ),
            (
                -- \x -> plus 1 x
                LAbs "x" ((LVar "plus") :$: (LVar "1") :$: (LVar "x")),
                "(.plus. 1)"
            ),
            (
                -- \x -> \y -> x y
                LAbs "x" (LAbs "y" ((LVar "x") :$: (LVar "y"))),
                "I"

            ),
            (
                -- \x -> \y -> y x
                LAbs "x" (LAbs "y" ((LVar "y") :$: (LVar "x"))),
                "(C I)"

            ),
            (
                -- \x -> \y -> \z -> x y z
                LAbs "x" (LAbs "y" (LAbs "z" (LVar "x" :$: LVar "y" :$: LVar "z"))),
                "I"

            ),
            (
                (LVar "x") :$: (LVar "y"),
                "(x y)"
            ),
            (
                LVar "x",
                "x"
            ),
            (
                LAbs "x" (LAbs "y" (LVar "y")),
                "(K I)"

            ),
            (
                LAbs "x" (LAbs "x" (LVar "x")),
                "(K I)"

            ),
            (
                LAbs "x" (LVar "y" :$: (LVar "x")),
                "y"

            )]

-- ==================================================
--
--  Interpreter
--
-- ==================================================

getTestCases :: FilePath -> IO [(String, String)]
getTestCases path = do 
        contents <- readFile path
        return (aux (lines contents))
    where
        aux []                       = []
        aux (('#':_):tail)           = aux tail
        aux (input:expected:[])      = [(input, expected)]
        aux (input:expected:"":tail) = (input, expected) : aux tail
        aux _                        = error "Invalid test case..."

testInterpreter :: IO ()
testInterpreter = do
    cases <- getTestCases "interpreter.tests"
    let result = compare' (evaluate . translate . parse) id cases in
        putStrLn (if result == "" then "Ok" else result)

-- ==================================================
--
--  Auxiliary
--
-- ==================================================

compare' :: (Show c, Eq c) => (a -> c) -> (b -> c) -> [(a, b)] -> String
compare' _ _ [] = ""
compare' ti te (x:xs) = check x ++ (compare' ti te xs)
    -- i = input, e = expected
    where check (i, e) = let ti' = ti i
                             te' = te e
                             in if ti' == te'
                                    then ""
                                    else show ti' ++ " !=  " ++ (show te') ++ "\n"
