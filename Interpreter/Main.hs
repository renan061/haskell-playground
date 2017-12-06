import System.Environment (getArgs)

import Parser
import Translator
import Evaluator

-- I(K(Ky)ab) -> y
-- todo = I :$: (K :$: (K :$: (LVar "y")) :$: (LVar "a") :$: (LVar "b"))

-- K(KI)abc -> c
-- todo = K :$: (K :$: I) :$: (LVar "a") :$: (LVar "b") :$: (LVar "c")

-- CIab -> ba
-- todo = C :$: I :$: (LVar "a") :$: (LVar "b")

-- main = putStrLn (evaluate todo)

-- repl :: IO ()
-- repl = do
--         line <- getLine
--         if line == "exit"
--             then putStrLn "Exiting..."
--             else do
--                 -- (putStrLn . evaluate . translate . parse) line
--                 putStrLn (evaluate (translate (parse line)))
--                 repl

-- main = repl

main = do args <- getArgs
          (putStrLn . lci . head) args
    where lci = evaluate . translate . parse
