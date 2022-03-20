type Symbol a = a
type Sentence a = [a]
-- | A context-free production
type Production a = (Symbol a, Sentence a)
-- | A deterministic L-system grammar. Consists of the start symbol and the productions
data DetGrammar a = DetGrammar (Symbol a) [Production a]
  deriving Show

detGenerate :: Eq a => DetGrammar a -> [Sentence a]
detGenerate (DetGrammar start productions) = iterate applyProductions [start]
  where applyProductions = concatMap applyProduction
        applyProduction symbol = maybe [symbol] id (findProduction symbol)
        findProduction symbol = lookup symbol productions

-- | Lindenmayer's L-system for modelling algae growth (https://en.wikipedia.org/wiki/L-system#Example_1:_Algae)
lindenmayerAlgae :: [Sentence Char]
lindenmayerAlgae = detGenerate $ DetGrammar 'A' [('A', ['A', 'B']), ('B', ['A'])]

genGrammarAndPrintFirstN :: Int -> DetGrammar Char -> IO ()
genGrammarAndPrintFirstN n grammar = mapM_ putStrLn $ take n $ detGenerate grammar

-- Exercise 13.2
-- | Test if a grammar is deterministic
testDet :: Eq a => DetGrammar a -> Bool
testDet (DetGrammar _ productions) = not $ haveMultipleProductions productions
  where haveMultipleProductions [] = False
        haveMultipleProductions (p:ps) = maybe (haveMultipleProductions ps) (const True) $ findSameProductions p ps
        findSameProductions p ps = lookup (fst p) ps

-- End exercise