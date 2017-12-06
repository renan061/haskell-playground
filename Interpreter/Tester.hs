import TranslatorTest

main = testTranslator

-- lprint :: (Maybe LExp, Maybe (LExp, String)) -> IO ()
-- lprint (Just x, Nothing) = print x 
-- lprint (Nothing, Just (x, err)) = putStrLn e
--     where e = "error:\n\twrong: " ++ (show x) ++ "\n\tunused: " ++ (show err)
-- lprint _ = error "error: internal"

-- main = do
--         lprint (lparser "x    ")
--         lprint (lparser "(x)")
--         lprint (lparser "\\x  ->  (x)  ")
--         lprint (lparser "(\\x -> x) y")
--         lprint (lparser "x y")
--         lprint (lparser "(\\x -> x y) x")
--         lprint (lparser "(\\x -> \\y -> x y) a b")
--         lprint (lparser "x #comentário")
--         lprint (lparser "(\\x -> \\y -> x y) a b # mais comentário")
--         lprint (lparser "(\\x -> \\y -> x y) a b # mais # comentário dentro")
--         lprint (lparser "(\\x -> \\y -> x y z) a b # mais # comentário dentro")
--         lprint (lparser "x (y z)")
--         lprint (lparser "x y z")
--         lprint (lparser "a b c d")
--         lprint (lparser "a (b c) d")
--         lprint (lparser "(\\n -> n) 10")
--         lprint (lparser "\\n -> n 10")
--         lprint (lparser "xyz10 abc11")
--         lprint (lparser "(a))") -- unmatched parentheses