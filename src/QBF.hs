module QBF where

import qualified Data.Set as S
import Data.List 
import Data.Maybe

-------------------------------------------------------------------------------
--                              Data declarations
-------------------------------------------------------------------------------

data Problem = Problem {num_atoms :: Int,
                        num_clauses :: Int,
                        qbf :: QBF} deriving (Eq, Ord)

type ID = Int

data Variable = Var {name :: ID} deriving (Eq, Ord)
data Quantifier = Exists {vars :: (S.Set Variable)} | Forall {vars :: (S.Set Variable)} deriving (Eq, Ord)

data Literal = Pos {atom :: Variable} | Neg {atom :: Variable} deriving (Eq, Ord)
type Clause = [Literal]
data QBF = QBF {quantifiers :: (Quantifier,Quantifier),
                clauses :: [Clause]} deriving (Eq, Ord)

-------------------------------------------------------------------------------
--                        Auxiliary functions
-------------------------------------------------------------------------------
-- TODO:
--  Optimize: create generic, composable functions that prevent
--  the need for multiple traversals 

toPicosat :: [Clause]-> [[Int]]
toPicosat cs = map (map fromLiteral)  cs

toLiteral :: Int -> Literal
toLiteral i = if i > 0 then Pos $ Var i else Neg $ Var (- i)

fromLiteral :: Literal -> Int
fromLiteral (Pos v) = name v
fromLiteral (Neg v) = - name v

complement :: Literal -> Literal
complement (Pos v) = Neg v
complement (Neg v) = Pos v

-- Query functions
universalsRight :: QBF -> S.Set Variable
universalsRight form = g $ quantifiers form
    where g (_,Forall v)    = v
          g _               = S.empty

existentialsLeft :: QBF -> S.Set Variable
existentialsLeft form =  g $ quantifiers form
    where   g (Exists v,_)    = v
            g  _              = S.empty

existentials :: QBF -> S.Set Variable
existentials form = g $ quantifiers form
    where   g (Exists v,_)    = v
            g (_,Exists v)    = v
            g _               = S.empty

universals :: QBF -> S.Set Variable
universals form = g $ quantifiers form
    where   g (Forall v,_)    = v
            g (_,Forall v)    = v
            g _               = S.empty

-- Search functions

-- Won't work for unit literals; context (clause) is required
genericFindLiterals :: (Literal -> Bool) -> QBF -> [Literal]
genericFindLiterals test form = concatMap (filter test) $ clauses form


findUnitLiterals :: QBF -> [Literal]
findUnitLiterals form = nub $ mapMaybe f $ clauses form
    where   e    = existentials form
            uR   = universalsRight form            
            f cl = let (eC,uC) = partition (flip elem e . atom) cl in 
                    if length (nub eC) == 1 && all (flip elem uR . atom) uC 
                        then Just (head eC)
                        else Nothing

findPureLiterals :: QBF -> [Literal]
findPureLiterals form = filter f lits
    where   lits    = concat $ clauses form
            f lit   = not $ (complement lit) `elem` lits
            

           
-- Functions for updating QBFs
removeClauses :: (Clause -> Bool) -> QBF -> QBF
removeClauses test (QBF qs cls) = QBF qs cls'
    where   cls' = filter (not . test) cls

removeLiterals :: (Literal -> Bool) -> QBF -> QBF
removeLiterals test  (QBF qs cls) = QBF qs cls'
    where   cls' = map (filter (not . test)) cls

removeQuantifiers :: (Variable -> Bool) -> QBF -> QBF
removeQuantifiers test (QBF (q1,q2) cls) = QBF (f q1,f q2) cls
    where   f (Exists v) = Exists $ g v
            f (Forall v) = Forall $ g v
            g            = S.filter (not . test)


-------------------------------------------------------------------------------
--                        Simplification functions
-------------------------------------------------------------------------------
-- TODO:
--  Optimize: create generic, composable functions that prevent
--  the need for multiple traversals (applies to auxiliary functions, too)

pureLiteralElimination :: Problem -> Problem
pureLiteralElimination (Problem a c form) = Problem a c form'    
    where   form'   = removeQuantifiers h . removeLiterals f . removeClauses g $ form
            purLits = findPureLiterals form
            (uP,eP) = partition (\l -> atom l `elem` universals form) purLits
            g cl    = any (flip elem cl) eP
            f lit   = lit `elem` uP
            h v     = Pos v `elem` purLits || Neg v `elem` purLits

unitLiteralElimination :: Problem -> Problem
unitLiteralElimination (Problem a c form) = Problem a c form'    
    where   form'   = removeQuantifiers h . removeLiterals f . removeClauses g $ form
            unitLiterals = findUnitLiterals form
            g cl         = any (flip elem cl) unitLiterals
            f lit        = complement lit `elem` unitLiterals
            h v          = Pos v `elem` unitLiterals || Neg v `elem` unitLiterals

-- TODO
universalReduction :: Problem -> Problem
universalReduction = undefined



-------------------------------------------------------------------------------
--                      Instance declations
-------------------------------------------------------------------------------

instance Show Variable where
    show (Var v) = show v

instance Show Literal where
    show (Pos v) = " " ++ show v
    show (Neg v) = "-" ++ show v

instance Show Quantifier where
    show (Exists vars) = "e " ++ (S.foldr f "0" vars)
        where f v r = show v ++ " " ++  r
    show (Forall vars) = "a " ++ (S.foldr f "0" vars)
        where f v r = show v ++ " " ++  r

instance Show QBF where
    show (QBF (q1,q2) cls) = show q1 ++ "\n" ++ show q2 ++ "\n"
        ++ foldr f "" cls
            where f cl r = foldr g "0\n" cl ++ r
                  g l r = show l ++ " " ++ r
instance Show Problem where
    show (Problem v q f) = "p cnf " ++ show v ++ " " ++ show q ++ "\n" ++ show f
