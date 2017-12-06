module Error (parserError, evaluatorError) where

import LExp

error' str = error ("\n\terror: " ++ str)

parserError unused message = 
    error' ("\n\t\tunused: " ++ (show unused) ++ "\n\t\terror: " ++ message)

evaluatorError e = error' ("unexpected " ++ show e)
