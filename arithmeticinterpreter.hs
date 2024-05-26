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

type Info = (Grammar, (Int, (Int, (Int, Int))))
type Table = [[[Info]]]

ckyInit :: [Char] -> Table
ckyInit xs = terminals ++ cky terminals (size - 1)
    where terminals = map (\x -> map (\y -> zip y (zip list (zip list (zip list list)))) x) [map (\x -> [fromTerminal x]) xs]
          size      = length xs
          list      = [0,0..size]

cky :: Table -> Int -> Table
cky xs 0 = []
cky xs n = line ++ cky (xs ++ line) (n - 1)
    where line = ckyLine xs n

ckyLine :: Table -> Int -> Table
ckyLine (x:xs) n = [concat [ckyFind (x:xs) n k | k <- [0..n - 1]]]

ckyFind :: Table -> Int -> Int -> [[Info]]
ckyFind (y:ys) n k = [concat [concat [[(fromNonTerminal (choice1', choice2''), (index1 - 1, (k, (size - n - index1, index2 - 1))))] | let choice2 = fst (lines !! (size - n - index1)), (choice2', index2) <- zip choice2 [1..], let choice2'' = getFirstSymbol choice2', index2 > k, let result = fromNonTerminal (choice1', choice2''), result /= None] | c <- lines, let (choice1, index1) = ((fst c) !! k, snd c), let choice1' = getFirstSymbol choice1]]
    where lines = zip (x:xs) [1..]
          size  = length x
          (x:xs) = map (map (map (fst))) (y:ys)
          getFirstSymbol :: [Grammar] -> Grammar
          getFirstSymbol []     = None
          getFirstSymbol (x:xs) = x

simplifyParsingTable :: Table -> Table
simplifyParsingTable []     = []
simplifyParsingTable [x]    = ((map (\y -> getFirstInitialSymbol y) x)):[]
simplifyParsingTable (x:xs) = x:simplifyParsingTable xs

getFirstInitialSymbol :: [Info] -> [Info]
getFirstInitialSymbol []             = []
getFirstInitialSymbol (x:xs) | fst x == initialSymbol = [x]
                             | otherwise              = getFirstInitialSymbol xs
    where initialSymbol = S

checkParsingSuccess :: Table -> Bool
checkParsingSuccess [] = False
checkParsingSuccess xs = or (map (any (\x -> (fst x) == S)) (last xs))

getTreeHead :: Table -> Info
getTreeHead [] = (None, (0, (0, (0, 0))))
getTreeHead xs = head $ head $ symbol
    where symbol = last xs

findSymbols :: Table -> Int -> Int -> Int -> Int -> (Info, Info)
findSymbols xs n k n' m = (head ((xs !! n) !! k), head ((xs !! n') !! m))

convertToTree :: Table -> Info -> Tree
convertToTree xs (g, (0, (0, (0, 0)))) = Leaf g
convertToTree xs (g, (n, (k, (n', m)))) = Node g (convertToTree xs (fst symbols)) (convertToTree xs (snd symbols))
    where symbols = findSymbols xs n k n' m

ckyTree :: [Char] -> Tree
ckyTree xs = convertToTree table head
    where table = simplifyParsingTable $ ckyInit xs
          head  = getTreeHead table
