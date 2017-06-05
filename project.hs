type Atom = Char
data Proposition = PropAtom Atom | And Proposition Proposition | Or Proposition Proposition | Not Proposition | Imp Proposition Proposition | DoubleImp Proposition Proposition deriving (Show)
type Evaluating = [(Atom,Bool)]
type Mosr = [Proposition]

evalAtom:: Evaluating->Atom->Bool
evalAtom list element = if (elem (element,True) list) then True else False

evaluate:: Proposition -> Evaluating -> Bool
evaluate (PropAtom a) eval = evalAtom eval a
evaluate (And a b) eval = (evaluate a eval) && (evaluate b eval)
evaluate (Or a b) eval = (evaluate a eval) || (evaluate b eval)
evaluate (Not a) eval = not (evaluate a eval)
evaluate (Imp a b) eval = if ((evaluate a eval)==True && (evaluate b eval)==False) then False else True
evaluate (DoubleImp a b) eval = if ((evaluate a eval)==(evaluate b eval)) then True else False

add:: Atom->[[(Atom,Bool)]]->[[(Atom,Bool)]]
add a [] = []
add a (x:xs) = ((a,False):x):((a,True):x):(add a xs) 

list:: [Atom]->[[(Atom,Bool)]]->[[(Atom,Bool)]]
list [] a = a
list (x:xs) a =  list xs (add x a)


listInit:: [Atom]->[[(Atom,Bool)]]
listInit (x:xs) = list xs ([(x,False)]:[(x,True)]:[])

pullAtoms:: Proposition->[Atom]->[Atom]
pullAtoms (PropAtom a) aux = if (elem a aux) then aux else (a:aux) 
pullAtoms (And a b) aux = pullAtoms b (pullAtoms a aux)
pullAtoms (Or a b) aux = pullAtoms b (pullAtoms a aux)
pullAtoms (Not a) aux = pullAtoms a aux
pullAtoms (Imp a b) aux = pullAtoms b (pullAtoms a aux)
pullAtoms (DoubleImp a b) aux = pullAtoms b (pullAtoms a aux)

giveValue:: Proposition->[[(Atom,Bool)]]->[Bool]
giveValue a b =  map (evaluate a) b

tautology::Proposition->Bool
tautology a = and (giveValue a (listInit (pullAtoms a [])))

contradiction::Proposition->Bool
contradiction a = and $ map (==False) (giveValue a (listInit (pullAtoms a [])))

contingency::Proposition->Bool
contingency a = if (((tautology a)==False) && ((contradiction a)==False)) then True else False
