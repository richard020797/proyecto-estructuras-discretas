type PropAtom = String
data Proposition = Atom PropAtom | Not Proposition | And Proposition Proposition | Or Proposition Proposition | Imp Proposition Proposition | Iff Proposition Proposition
	deriving(Show)


type Valuation = [(PropAtom, Bool)]

--evalAtom :: Valuation -> Proposition -> Bool
--evalAtom (Atom x) v = 
--f [] = True
--f [(x, y) : xs] = x : y : f xs



--eval :: Valuation -> Proposition -> Bool
--eval (Atom x) v = evalAtom x v
--eval (Not x) v = not (eval x v)
--eval (And x y) v = && (eval x v) (eval y v)
--eval (Or x y) v = || (eval x v) (eval y v)
--eval (Imp x y) v = imp (eval x v) (eval y v)
--eval (Iff x y) v =  iff (eval x v) (eval y v)

-- valuations :: [PropAtom] -> [Valuation]
-- valuations (x:y) = 
