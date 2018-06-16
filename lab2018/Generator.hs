-- LABORATORIO DE PROGRAMACION FUNCIONAL 2018
-- MODULO DE GENERACION DE CODIGO DE MAQUINA


-- Se debe implementar la funcion generate, que dado
-- un AST que representa a un programa valido genera
-- el AST del codigo de maquina correspondiente

module Generator where


import Syntax
import MachineLang
import Debug.Trace
import TypeChecker
import Optimizer
-- se pueden agregar mas importaciones
-- en caso de ser necesario

debug = flip trace

generate :: Program -> Code
generate (Program name defs body) = (generadorCode defs body) --`debug` "generate" 

generadorCode :: Defs -> Body -> Code
generadorCode def [] = [] --`debug` "generateCode []"
generadorCode def ((Write exp):xs) = [WRITE] ++ generadorExpr def exp ++ generadorCode def xs 
generadorCode def ((While exp body):xs) = generadorExpr def exp ++ generadorCode def body ++ generadorCode def xs
generadorCode def ((If exp body1 body2):xs) = generadorExpr def exp ++ generadorCode def body1 ++ generadorCode def body2 ++ generadorCode def xs   
generadorCode def ((Read nam):xs) = [READ] ++ generadorCode def xs
generadorCode def ((Assig name exp):xs) = generadorExpr def exp ++ generadorCode def xs 

generadorExpr :: Defs -> Expr -> Code
generadorExpr def (Var n) = [STORE n] 
generadorExpr def (BoolLit b) = if b == True then [PUSH 1] else [PUSH 0] 
generadorExpr def (IntLit b) = [PUSH b] 
generadorExpr def (Unary Not exp) = [NEG] 
--generadorExpr def ((Binary op exp1 exp2):xs) = 
    
    

aux :: String -> Code
aux a = undefined
                                      
