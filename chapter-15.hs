inOut :: IO ()
inOut = do s <- getLine
           putStrLn s

putStr' :: String -> IO ()
putStr' = sequence_ . map putChar

putStr'' :: String -> IO ()
putStr'' [] = return ()
putStr'' (c:cs) = do 
  putChar c
  putStr'' cs