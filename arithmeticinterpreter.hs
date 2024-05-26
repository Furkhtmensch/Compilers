data Grammar = S | P | PE | PD | M | M0 | V | V0 | Error
    deriving (Show, Eq)

data Tree = Leaf Grammar | Node Grammar Tree Tree

fromTerminal :: Char -> Grammar
fromTerminal '0' = S
fromTerminal '1' = S
fromTerminal '(' = PE
fromTerminal ')' = PD
fromTerminal '+' = M0
fromTerminal '*' = V0

fromNonTerminal :: (Grammar, Grammar) -> [Grammar]
fromNonTerminal (P, PD) = [S]
fromNonTerminal (M, S)  = [S]
fromNonTerminal (V, S)  = [S]
fromNonTerminal (PE, S) = [P]
fromNonTerminal (S, M0) = [M]
fromNonTerminal (S, V0) = [V]
fromNonTerminal x       = []

ckyInit :: [Char] -> [[[Grammar]]]
ckyInit xs = terminals ++ cky terminals (length xs - 1)
    where terminals = [map (\x -> [fromTerminal x]) xs]

cky :: [[[Grammar]]] -> Int -> [[[Grammar]]]
cky xs 0 = []
cky xs n = line ++ cky (xs ++ line) (n - 1)
    where line = ckyLine xs n

ckyLine :: [[[Grammar]]] -> Int -> [[[Grammar]]]
ckyLine (x:xs) n = [concat [ckyFind (x:xs) n k | k <- [0..n - 1]]]

ckyFind :: [[[Grammar]]] -> Int -> Int -> [[Grammar]]
ckyFind (x:xs) n k = [concat [concat [fromNonTerminal (choice1', choice2'') | let choice2 = fst (lines !! (size - n - index1)), (choice2', index2) <- zip choice2 [0..], choice2'' <- choice2', index2 > k] | c <- lines, let (choice1, index1) = ((fst c) !! k, snd c), choice1' <- choice1]]
    where lines = zip (x:xs) [1..]
          size  = length x
