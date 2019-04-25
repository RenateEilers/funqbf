module Solver where
import QBF
import Picosat
import Data.List
import Data.Set (toList)
import System.Random 

-------------------------------------------------------------------------------
--                     Data declarations 
-------------------------------------------------------------------------------

data Result = SAT | UNSAT deriving (Show, Eq)
type SolverResult = IO Result

-------------------------------------------------------------------------------
--                      Auxiliary functions
-------------------------------------------------------------------------------

getSolution :: [Clause] -> [Int] -> IO (Result,[Int])
getSolution cls assums =  do
        sol <-  evalScopedPicosat $ do
                    addBaseClauses $ toPicosat cls
                    scopedSolutionWithAssumptions $ assums
        return $ f sol
            where f (Solution s) = (SAT,s)
                  f _            = (UNSAT,[])

negateQBF:: Int -> [Clause] -> (Int,[Clause])
negateQBF v cls = freshVars $ foldr f (v,[]) cls
    where   f cl (i,clss) = (i+1,(negateClause (toLiteral (-(i+1))) cl) ++ clss )            
            freshVars (i,cls) = (i,(map toLiteral [v+1..i]) : cls)

negateClause :: Literal -> Clause -> [Clause]
negateClause freshLit cl = foldr f [] cl
    where f lit cls  = [freshLit,complement lit] : cls

assignVars :: [Literal] -> [Clause] ->  [Clause]
assignVars lits cls =  map (filter $ not . h) . filter (null . intersect lits) $ cls            
    where h        = flip elem $ map complement lits
          
-------------------------------------------------------------------------------
--                      Expansion-based solving
-------------------------------------------------------------------------------

expansionSolve :: Problem -> SolverResult
expansionSolve p = do   g <- getStdGen
                        let r = repeat 1 --randomRs (0,1 :: Int) g
                        let us = toList $ universals $ qbf p 
                        let alpha = zipWith f r us                        
                        expansionSolveSub p alpha []
        where   f i v = if (i == 0) then -(name v) else name v


expansionSolveSub :: Problem -> [Int] -> [Clause] -> SolverResult
expansionSolveSub p alpha psi = do
        let q = clauses $ qbf p
        (resTau,tau') <- getSolution q alpha   
        let tau = (filter (flip elem (existentials $ qbf p) . atom . toLiteral)  tau')                             
        --putStr "alpha: "
        --print alpha 
        --putStr "psi: "
        --print psi
        --putStr "tau:"
        --print tau     
        if resTau == UNSAT
            then return UNSAT 
            else do                                     
                let (numVars,notP) = negateQBF (num_atoms p) q                        
                let notPTau = assignVars (map toLiteral tau) $ notP
                let psi' = psi ++ notPTau
                --print $ negateQBF (num_atoms p) q
                --putStr "psi': "
                --print psi' 
                (resAlpha,alphaAll) <- getSolution psi' []
                --putStr "alpha': "  
                let alpha' = filter (flip elem (universals $ qbf p) . atom . toLiteral)  alphaAll                               
                --print alpha'                
                if resAlpha == UNSAT
                    then return SAT                    
                    else do 
                        let p' = Problem numVars (num_clauses p) (qbf p) 
                        expansionSolveSub p' alpha' psi'