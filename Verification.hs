module Verification where

import LTL
import Kripke
import SExpr

import Data.Set (Set)
import qualified Data.Set as Set


-- an Assertion is a model, state, formula set triple with a fancy symbol in the middle.
data Assertion = (KripkeModel, State) :|- (Set LTL) deriving (Eq, Show)

-- Cycle-detecting verification algorithm for a given Assertion
checkLTL :: Assertion -> Bool
checkLTL s = dfs s []
  where
    dfs s@((km, st) :|- phi) l
      | elem s l  = checkCycle (s:(takeWhile (/= s) l))
      | otherwise = dfsSub (subgoals s) (s:l)

    dfsSub (Left True) _ = True
    dfsSub (Right [])  _ = False
    dfsSub (Right sg)  l = and $ map (flip dfs l) sg
    
    checkCycle phi = not $ null [TR p q | TR p q <- phi', not $ elem q phi']
      where phi' = Set.toList $ Set.unions [p | (_,_) :|- p <- phi]

    subgoals s@((km, st) :|- phi) 
      | null phi = Right []
      | otherwise = 
          let s'   = Set.elemAt 0 phi 
              phi' = Set.delete s' phi
           in case s' of
                TLit p   
                  | p `holdsIn` (km, st) -> Left True 
                  | otherwise -> Right [(km, st) :|- phi']
                TNeg p
                  | not $ p `holdsIn` (km, st) -> Left True
                  | otherwise -> subgoals $ (km,st) :|- phi'
                TOr p q  -> Right [(km, st) :|- (Set.union (Set.fromList [p, q]) phi')]
                TAnd p q 
                  | p == q    -> Right [(km, st) :|- (Set.insert p phi')]
                  | p < q     -> Right [
                                  (km, st) :|- (Set.insert p phi'), 
                                  (km, st) :|- (Set.insert q phi')]
                  | otherwise -> Right [
                                  (km, st) :|- (Set.insert q phi'), 
                                  (km, st) :|- (Set.insert p phi')]
                TU p q   
                  | p == q    -> Right [(km, st) :|- Set.insert p phi']
                  | otherwise -> Right [ 
                              (km, st) :|- (Set.union (Set.fromList [p, q]) phi'), 
                              (km, st) :|- (Set.union (Set.fromList [q, TX (TU p q)]) phi')]
                TR p q   
                  | p == q    -> Right [(km, st) :|- Set.insert p phi']
                  | otherwise -> Right [
                              (km, st) :|- (Set.insert q phi'),
                              (km, st) :|- (Set.union (Set.fromList [p, TX (TR p q)]) phi')]
                TX _     -> Right $ let phis = Set.map (\(TX p) -> p) phi
                                     in map (\s' -> (km, s') :|- phis) $ neighbors km st

parseK p = let Just s = parseSExpr p in parseKripke s
