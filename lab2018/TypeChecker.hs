-- LABORATORIO DE PROGRAMACION FUNCIONAL 2018
-- MODULO DE CHEQUEO DE NOMBRES Y TIPOS

-- Se debe implementar la funcion checkProgram, que
-- realiza los chequeos de nombres y tipos como se
-- especifica en la letra

module TypeChecker where

import Syntax
import Debug.Trace
-- se pueden agregar mas importaciones 
-- en caso de ser necesario


-- TYPE CHECKER

data Error = Duplicated      Name
           | Undefined       Name
           | Expected        Type Type

instance Show Error where
 show (Duplicated      n)  = "Duplicated definition: " ++ n
 show (Undefined       n)  = "Undefined: " ++ n
 show (Expected    ty ty') = "Expected: " ++ show ty ++ " Actual: " ++ show ty'


debug = flip trace

checkProgram :: Program -> [Error]
checkProgram (Program name defs body) = if null (checkDupNames defs) then
                                            (checkUndefined (defListaNames defs) body) --`debug` "Anduvo aca"
                                        else  
                                            (checkDupNames defs) --`debug` "O anduvo aca"


-- Nombres duplicados
checkDupNames :: Defs -> [Error]
checkDupNames def = if null repetidos then 
                        (repetidos) --`debug` "Anduvo aca 1" 
                    else 
                        (repetidos) --`debug` "O anduvo aca 1"
                    where repetidos = [Duplicated x | x <- reverse(isDup $ reverse(defToNames def))]

isDup :: [Name] -> [Name] 
isDup [] = []
isDup (x:xs) = if x `elem` xs then [x] ++ isDup(xs) else isDup(xs)

defToNames :: Defs -> [Name]
defToNames [] = []
defToNames ((VarDef n ty):xs) = [n] ++ defToNames xs


-- No definidos
checkUndefined :: [Name] -> Body -> [Error]
checkUndefined _ [] = []
checkUndefined nam ((Read x):xs) = if not(x `elem` nam) then [Undefined x] ++ (checkUndefined nam xs) else checkUndefined nam xs
checkUndefined nam ((Write exp):xs) = checkUndefinedExp nam (expListaNames [exp]) ++ checkUndefined nam xs
checkUndefined nam ((While exp body):xs) = checkUndefinedExp nam (expListaNames [exp]) ++ checkUndefined nam body ++ checkUndefined nam xs
checkUndefined nam ((Assig n exp):xs) = if (n `elem` nam) then checkUndefinedExp nam (expListaNames [exp]) ++ checkUndefined nam xs
                                        else [Undefined n] ++ checkUndefinedExp nam (expListaNames [exp]) ++ checkUndefined nam xs
checkUndefined nam ((If exp body1 body2):xs) = checkUndefinedExp nam (expListaNames [exp]) ++ checkUndefined nam body1 ++ checkUndefined nam body2 ++ checkUndefined nam xs


checkUndefinedExp :: [Name] -> [Name] -> [Error]
checkUndefinedExp _ [] = []
checkUndefinedExp namE (x:xs) = if (x `elem` namE) then checkUndefinedExp namE xs
									else [Undefined x] ++ checkUndefinedExp namE xs

defListaNames :: Defs -> [Name]
defListaNames [] = []
defListaNames ((VarDef n ty):xs) = [n] ++ defListaNames xs	

expListaNames :: [Expr] -> [Name]
expListaNames [] = []
expListaNames ((Var n):xs) = [n] ++ expListaNames xs
expListaNames ((IntLit _):xs) = [] ++ expListaNames xs
expListaNames ((BoolLit _):xs) = [] ++ expListaNames xs	
expListaNames ((Unary _ exp):xs) = expListaNames [exp] ++ expListaNames xs
expListaNames ((Binary _ exp1 exp2):xs) = expListaNames [exp1] ++ expListaNames [exp2] ++ expListaNames xs

aux :: String -> [Error]
aux a = undefined  
