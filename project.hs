module Libr where

type Atomica = Char
data Proposicion = PropAtomica Atomica | And Proposicion Proposicion | Or Proposicion Proposicion | No Proposicion | Implica Proposicion Proposicion | DobleImplica Proposicion Proposicion deriving (Show)
type Evaluador = [(Atomica,Bool)]
type Despliegue = [Proposicion]

evaluarAtom:: Evaluador->Atomica->Bool
evaluarAtom lista elemento = if (elem (elemento,True) lista) then True else False

evaluar:: Proposicion -> Evaluador -> Bool
evaluar (PropAtomica a) eval = evaluarAtom eval a
evaluar (And a b) eval = (evaluar a eval) && (evaluar b eval)
evaluar (Or a b) eval = (evaluar a eval) || (evaluar b eval)
evaluar (No a) eval = not (evaluar a eval)
evaluar (Implica a b) eval = if ((evaluar a eval)==True && (evaluar b eval)==False) then False else True
evaluar (DobleImplica a b) eval = if ((evaluar a eval)==(evaluar b eval)) then True else False

agregar:: Atomica->[[(Atomica,Bool)]]->[[(Atomica,Bool)]]
agregar a [] = []
agregar a (x:xs) = ((a,False):x):((a,True):x):(agregar a xs) 

lista:: [Atomica]->[[(Atomica,Bool)]]->[[(Atomica,Bool)]]
lista [] a = a
lista (x:xs) a =  lista xs (agregar x a)

listaInit:: [Atomica]->[[(Atomica,Bool)]]
listaInit (x:xs) = lista xs ([(x,False)]:[(x,True)]:[])

sacarAtomos:: Proposicion->[Atomica]->[Atomica]
sacarAtomos (PropAtomica a) aux = if (elem a aux) then aux else (a:aux) 
sacarAtomos (And a b) aux = sacarAtomos b (sacarAtomos a aux)
sacarAtomos (Or a b) aux = sacarAtomos b (sacarAtomos a aux)
sacarAtomos (No a) aux = sacarAtomos a aux
sacarAtomos (Implica a b) aux = sacarAtomos b (sacarAtomos a aux)
sacarAtomos (DobleImplica a b) aux = sacarAtomos b (sacarAtomos a aux)

valuando:: Proposicion->[[(Atomica,Bool)]]->[Bool]
valuando a b =  map (evaluar a) b

tautologia::Proposicion->Bool
tautologia a = and (valuando a (listaInit (sacarAtomos a [])))

contradiccion::Proposicion->Bool
contradiccion a = and $ map (==False) (valuando a (listaInit (sacarAtomos a [])))

contingencia::Proposicion->Bool
contingencia a = if (((tautologia a)==False) && ((contradiccion a)==False)) then True else False
