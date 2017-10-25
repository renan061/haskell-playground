getln :: IO String
getln = do c <- getChar;
           if c == '\n'
            then return ""
            else do cs <- getln
                    return (c:cs)

sameln :: String -> IO ()
sameln previous = do ln <- getln
                     if ln == previous
                        then print "equal"
                        else sameln ln

main = sameln ""
