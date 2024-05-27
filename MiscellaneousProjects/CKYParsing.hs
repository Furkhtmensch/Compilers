data Grammar = S | P | PE | PD | M | M0 | V | V0 | None
    deriving (Show, Eq)

data Tree = Leaf Grammar | Node Grammar Tree Tree
    deriving (Show)

fromTerminal :: Char -> Grammar
fromTerminal '0' = S
fromTerminal '1' = S
fromTerminal '(' = PE
fromTerminal ')' = PD
fromTerminal '+' = M0
fromTerminal '*' = V0

fromNonTerminal :: (Grammar, Grammar) -> Grammar
fromNonTerminal (P, PD) = S
fromNonTerminal (M, S)  = S
fromNonTerminal (V, S)  = S
fromNonTerminal (PE, S) = P
fromNonTerminal (S, M0) = M
fromNonTerminal (S, V0) = V
fromNonTerminal x       = None

type Table = [[[Grammar]]]

ckyInit :: [Char] -> Table
ckyInit xs = terminals ++ cky terminals (size - 1)
    where terminals = [map (\x -> [fromTerminal x]) xs]
          size      = length xs
          list      = [0,0..size]

cky :: Table -> Int -> Table
cky xs 0 = []
cky xs n = line ++ cky (xs ++ line) (n - 1)
    where line = ckyLine xs n

ckyLine :: Table -> Int -> Table
ckyLine (x:xs) n = [concat [ckyFind (x:xs) n k | k <- [0..n - 1]]]

ckyFind :: Table -> Int -> Int -> [[Grammar]]
ckyFind (x:xs) n k = [concat [concat [[fromNonTerminal (choice1', choice2'')] | let choice2 = fst (lines !! (size - n - index1)), (choice2', index2) <- zip choice2 [1..], let choice2'' = getFirstSymbol choice2', index2 > k, let result = fromNonTerminal (choice1', choice2''), result /= None] | c <- lines, let (choice1, index1) = ((fst c) !! k, snd c), let choice1' = getFirstSymbol choice1]]
    where lines = zip (x:xs) [1..]
          size  = length x
          getFirstSymbol :: [Grammar] -> Grammar
          getFirstSymbol []     = None
          getFirstSymbol (x:xs) = x

simplifyParsingTable :: Table -> Table
simplifyParsingTable []     = []
simplifyParsingTable [x]    = ((map (\y -> getFirstInitialSymbol y) x)):[]
simplifyParsingTable (x:xs) = x:simplifyParsingTable xs

getFirstInitialSymbol :: [Grammar] -> [Grammar]
getFirstInitialSymbol []             = []
getFirstInitialSymbol (x:xs) | x == initialSymbol = [x]
                             | otherwise              = getFirstInitialSymbol xs
    where initialSymbol = S

checkParsingSuccess :: Table -> Bool
checkParsingSuccess [] = False
checkParsingSuccess xs = or (map (any (\x -> x == S)) (last xs))
