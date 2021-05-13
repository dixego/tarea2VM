module Kripke where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import SExpr 


type State = Integer
-- Type representing a Kripke model.
data KripkeModel = KripkeModel
  { graph  :: Map State [State]      -- Graph (States are identified by integers,
                                     -- with a list of their neighbors)
  , values :: Map State (Set String) -- Valuation for each state (which propositional 
                                     -- variables are true for a given state
  } deriving (Eq, Show)

-- S-Expression parser for a Kripke model
-- A Kripke model S-Expression has the form
-- K := (model (n (p..) (n ...)) ...)
-- i.e. an S-Expression starting with the symbol `model`, then one or more S-Expressions
-- with a state id (integer), a (possibly empty) list of propositional variables, and a
-- (possibly empty) list of neighboring state ids. For example, the following S-Expr:
-- 
-- (model (1 (p q) (2)) (2 (r) (1 2)))
-- is a model with two states:
-- - state 1 where `p` and `q` are true, with an edge towards state 2
-- - state 2 where `r` is true, with an edge towards itself and towards state 1
parseKripke :: SExpr -> KripkeModel
parseKripke (List (Sym "model":states)) = KripkeModel { graph = pStates, values = pVals }
  where
    (pStates, pVals) = foldl parseState (Map.empty, Map.empty) states
    parseState (g, v) (List [Num n, List vals, List ns]) = (g', v')
      where g' = Map.insert n (map (\(Num m) -> m) ns) g
            v' = Map.insert n (Set.fromList $ map (\(Sym s) -> s) vals) v
    parseState _ _ = error "Not a valid Kripke model."

parseKripke _ = error "Not a valid Kripke model."

-- determines if `p` is true in state `s` of model `k` via set membership
holdsIn :: String -> (KripkeModel, State) -> Bool
holdsIn p (k, s) = Set.member p ((values k) Map.! s)

-- returns the list of neighbors for state `s` in model `m`
neighbors :: KripkeModel -> State -> [State]
neighbors m s = (graph m) Map.! s

--
