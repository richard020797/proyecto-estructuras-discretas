type Atom = Char
data Proposition = PropAtom Atom | And Proposition Proposition | Or Proposition Proposition | Not Proposition | Implica Proposition Proposition | DobleImplica Proposition Proposition deriving (Show)
type Evaluating = [(Atom,Bool)]
type Show = [Proposition]

evalAtom:: Evaluating->Atom->Bool
evalAtom list elemento = if (elem (elemento,True) list) then True else False

eval:: Proposition -> Evaluating -> Bool
eval (PropAtom a) eval = evalAtom eval a
eval (And a b) eval = (eval a eval) && (eval b eval)
eval (Or a b) eval = (eval a eval) || (eval b eval)
eval (Not a) eval = not (eval a eval)
eval (Implica a b) eval = if ((eval a eval)==True && (eval b eval)==False) then False else True
eval (DobleImplica a b) eval = if ((eval a eval)==(eval b eval)) then True else False

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
pullAtoms (Implica a b) aux = pullAtoms b (pullAtoms a aux)
pullAtoms (DobleImplica a b) aux = pullAtoms b (pullAtoms a aux)

EvaluateProposition:: Proposition->[[(Atom,Bool)]]->[Bool]
EvaluateProposition a b =  map (eval a) b

tautology::Proposition->Bool
tautology a = and (EvaluateProposition a (listInit (pullAtoms a [])))

contradiction::Proposition->Bool
contradiction a = and $ map (==False) (EvaluateProposition a (listInit (pullAtoms a [])))

contingency::Proposition->Bool
contingency a = if (((tautology a)==False) && ((contradiction a)==False)) then True else False
