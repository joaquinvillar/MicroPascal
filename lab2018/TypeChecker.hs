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
checkProgram (Program name defs body) = if True then
                                            (checkDupNames defs) --`debug` "Anduvo aca"
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