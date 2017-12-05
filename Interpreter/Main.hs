import Evaluator
import LExp -- TODO: Remove

-- I(K(Ky)ab) -> y
todo = I :$: (K :$: (K :$: (LVar "y")) :$: (LVar "a") :$: (LVar "b"))

main = putStrLn (evaluate todo)
