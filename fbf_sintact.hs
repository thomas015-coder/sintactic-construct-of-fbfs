data FBF = Atom Char
         | No FBF
         | Conj FBF FBF
         | Disj FBF FBF
         | Imp FBF FBF
         | Sys FBF FBF
         deriving(Eq)

instance Show FBF where 
  show (Atom c)  = [c]
  show (No f)       = "¬(" ++ show f ++ ")"
  show (Conj f1 f2) = "(" ++ show f1 ++ ") ∧ (" ++ show f2 ++ ")"
  show (Disj f1 f2) = "(" ++ show f1 ++ ") ∨ (" ++ show f2 ++ ")"
  show (Imp f1 f2)  = "(" ++ show f1 ++ ") → (" ++ show f2 ++ ")"
  show (Sys f1 f2)  = "(" ++ show f1 ++ ") ↔ (" ++ show f2 ++ ")"

longitud :: FBF -> Int
longitud (Atom _) = 1
longitud (No f) = 3 + longitud f
longitud (Conj f1 f2) = 5 + longitud f1 + longitud f2
longitud (Disj f1 f2) = 5 + longitud f1 + longitud f2
longitud (Imp f1 f2) = 5 + longitud f1 + longitud f2
longitud (Sys f1 f2) = 5 + longitud f1 + longitud f2

fbfsDeLongitud :: [Char] -> Int -> [FBF]
fbfsDeLongitud atoms n 
    | n <= 0    = []
    | n == 1    = [Atom c | c <- atoms]
    | otherwise  = neg ++ conj ++ disj ++ imp ++ sys 
  where
    neg  = [No f | f <-fbfsDeLongitud atoms (n-3)]

    bin op = [op f1 f2 | i <- [1 .. n-6], f1 <- fbfsDeLongitud atoms i, f2 <- fbfsDeLongitud atoms (n-5-i)]
    conj = bin Conj
    disj = bin Disj
    imp  = bin Imp
    sys  = bin Sys
 

generaTodasFbfs :: [Char] -> Int -> [FBF]
generaTodasFbfs atoms maxLen = concat [fbfsDeLongitud atoms n | n <- [1 .. maxLen]]
