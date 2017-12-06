import System.Environment (getArgs)

import Parser
import Translator
import Evaluator

main = do args <- getArgs
          (putStrLn . lci . head) args
    where lci = evaluate . translate . parse
