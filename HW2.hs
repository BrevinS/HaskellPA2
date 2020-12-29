module HW2
     where

-- 1
{- (a) merge2 5%-}
merge2::[a]->[a]->[a]
merge2 [] [] = []
merge2 [] (y:ys) = (y:ys)
merge2 (x:xs) [] = (x:xs)
merge2 (x:xs) (y:ys) = x:(y:(merge2 xs ys))
                         
{- (b) merge2Tail 10% -}
merge2Tail::[a]->[a]->[a]
merge2Tail l1 l2 = let
                    helper [] [] acc = (reverse acc)
                    helper [] (y:ys) acc = (reverse acc) ++ (y:ys)
                    helper (x:xs) [] acc = (reverse acc) ++ (x:xs)
                    helper (x:xs) (y:ys) acc = (helper xs ys (y:x:acc))
                   in helper l1 l2 [] 

{- (c) mergeN 5%-}
mergeN::[[a]]->[a]
mergeN ([]:ys) = []
mergeN ((x:xs):ys) = foldl merge2 (x:xs) ys

-- 2
{- (a) removDuplicates 10% -}
removeDuplicates::(Eq a)=>[a]->[a]  
removeDuplicates l1 = foldl (\acc x -> if x `elem` acc then acc else x:acc) [] l1

{- (b) count 5% -}
count::Eq a => a -> [a] -> Int
count z l1 = length $ filter (== z) l1

{- (c) histogram 10% -}
histogram::Eq a => [a] -> [(a, Int)]
histogram l1 = removeDuplicates (map func l1)
               where 
                    func x = (x, count x l1)   

-- 3                
{- (a) concatAll 4% -}
concatAll::[[String]] -> String
concatAll l1 = foldr (++) [] $ foldr (++) [] l1

{- (b) concat2Either 9% -}               
data AnEither  = AString String | AnInt Int
                deriving (Show, Read, Eq)

--concat2Either::[[AnEither]] -> AnEither
--concat2Either l1 = foldr (++) [] $ foldr (++) [] (AnEither l1)


{- (b) concat2Str 6% -}               




-- 4 

data Op = Add | Sub | Mul | Pow
          deriving (Show, Read, Eq)

evaluate:: Op -> Int -> Int -> Int
evaluate Add x y =  x+y
evaluate Sub x y =  x-y
evaluate Mul x y =  x*y
evaluate Pow x y = x^y

data ExprTree a = ELEAF a | ENODE Op (ExprTree a) (ExprTree a)
                  deriving (Show, Read, Eq)

{- (a) evaluateTree - 10% -}
evaluateTree:: ExprTree Int -> Int
evaluateTree (ELEAF v) = v
--This can't be how you're suppose to do this 
evaluateTree (ENODE Add t1 t2) = evaluateTree t1 + evaluateTree t2
evaluateTree (ENODE Sub t1 t2) = evaluateTree t1 - evaluateTree t2
evaluateTree (ENODE Mul t1 t2) = evaluateTree t1 * evaluateTree t2
evaluateTree (ENODE Pow t1 t2) = evaluateTree t1 ^ evaluateTree t2 
                              

{- (b) printInfix - 10% -}
printInfix::Show a => ExprTree a -> String
printInfix (ELEAF v) = show v
printInfix (ENODE Add t1 t2) = "(" ++ printInfix t1 ++ " " ++ "`Add`" ++ " " ++ printInfix t2 ++ ")"
printInfix (ENODE Sub t1 t2) = "(" ++ printInfix t1 ++ " " ++ "`Sub`" ++ " " ++ printInfix t2 ++ ")"
printInfix (ENODE Mul t1 t2) = "(" ++ printInfix t1 ++ " " ++ "`Mul`" ++ " " ++ printInfix t2 ++ ")"
printInfix (ENODE Pow t1 t2) = "(" ++ printInfix t1 ++ " " ++ "`Pow`" ++ " " ++ printInfix t2 ++ ")"

{- (c) createRTree 12% -}
data ResultTree a  = RLEAF a | RNODE a (ResultTree a) (ResultTree a)
                     deriving (Show, Read, Eq)

--createRTree:: ExprTree Int -> ResultTree Int
--createRTree (ELEAF v) = "( ELEAF" ++ show v ++ " )"
--createRTree (ENODE Add t1 t2) = "RNODE" show createRTree (evaluateTree t1 + evaluateTree t2) --createRTree t1 createRTree t2
--createRTree (ENODE Sub t1 t2) = "RNODE" show createRTree (evaluateTree t1 - evaluateTree t2) --createRTree t1 createRTree t2
--createRTree (ENODE Mul t1 t2) = "RNODE" show createRTree (evaluateTree t1 * evaluateTree t2) --createRTree t1 createRTree t2
--createRTree (ENODE Pow t1 t2) = "RNODE" show createRTree (evaluateTree t1 ^ evaluateTree t2) --createRTree t1 createRTree t2


-- 5
{-Sample trees 4% -}
--IN SAMPLETESTS
--sampletree1 = (ENODE Pow (ENODE Add 
                             -- (ENODE Min (ENODE Min (ELEAF 20) (ENODE Pow (ELEAF 4) (ELEAF 2))) (ENODE Pow (ELEAF 3) (ELEAF 3))) 
                                  -- (ENODE Min (ELEAF 6000) (ELEAF 5998))) 
                                      --  (ENODE Sub (ELEAF 10) (ELEAF 8)))
--sampletree2 = (ENODE Add (ELEAF 10) (ENODE Sub (ELEAF 50) (ENODE Mul (ELEAF 3) (ELEAF 10))))
--sampletree3 = (ENODE Mul (ENODE Sub (ENODE Add (ELEAF 4) (ELEAF 5)) (ELEAF 6)) (ENODE Sub (ELEAF 10) (ELEAF 8)))
--sampletree4 = (ENODE Add (ELEAF 10) (ENODE Sub (ELEAF 50) (ENODE Mul (ELEAF 3) (ELEAF 10))))







