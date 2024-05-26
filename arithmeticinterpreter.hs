data Grammar = S | P | PE | PD | M | M0 | V | V0 | None
    deriving (Show, Eq)

data Tree = Leaf Grammar | Node Grammar Tree Tree

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

ckyInit :: [Char] -> [[[(Grammar, Int)]]]
ckyInit xs = terminals ++ cky terminals (length xs - 1)
    where terminals = map (\x -> map (\y -> zip y [1,1..]) x) [map (\x -> [fromTerminal x]) xs]

cky :: [[[(Grammar, Int)]]] -> Int -> [[[(Grammar, Int)]]]
cky xs 0 = []
cky xs n = line ++ cky (xs ++ line) (n - 1)
    where line = ckyLine xs n

ckyLine :: [[[(Grammar, Int)]]] -> Int -> [[[(Grammar, Int)]]]
ckyLine (x:xs) n = [concat [ckyFind (x:xs) n k | k <- [0..n - 1]]]

ckyFind :: [[[(Grammar, Int)]]] -> Int -> Int -> [[(Grammar, Int)]]
ckyFind (y:ys) n k = [concat [concat [[(fromNonTerminal (choice1', choice2''), index1)] | let choice2 = fst (lines !! (size - n - index1)), (choice2', index2) <- zip choice2 [0..], let choice2'' = getFirstSymbol choice2', index2 > k, let result = fromNonTerminal (choice1', choice2''), result /= None] | c <- lines, let (choice1, index1) = ((fst c) !! k, snd c), let choice1' = getFirstSymbol choice1]]
    where lines = zip (x:xs) [1..]
          size  = length x
          (x:xs) = map (map (map (fst))) (y:ys)
          getFirstSymbol :: [Grammar] -> Grammar
          getFirstSymbol []     = None
          getFirstSymbol (x:xs) = x

