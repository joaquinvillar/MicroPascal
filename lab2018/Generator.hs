-- LABORATORIO DE PROGRAMACION FUNCIONAL 2018
-- MODULO DE GENERACION DE CODIGO DE MAQUINA


-- Se debe implementar la funcion generate, que dado
-- un AST que representa a un programa valido genera
-- el AST del codigo de maquina correspondiente

module Generator where


<<<<<<< HEAD
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
                                      
=======
    import Syntax
    import MachineLang
    import Optimizer
    -- se pueden agregar mas importaciones
    -- en caso de ser necesario
    
    generate :: Program -> Code
    generate (Program name defs body) = (generadorCode body) --`debug` "generate" 
    
    generadorCode :: Body -> Code
    generadorCode [] = [] `debug` "generateCode []"
    generadorCode ((Write exp):xs) =  generadorCode xs ++ generadorExpr exp ++ [WRITE] 
    generadorCode ((While exp body):xs) = generadorExpr exp  ++ generadorCode body ++ generadorCode xs 
    generadorCode ((If exp body1 body2):xs) = generadorExpr exp `debug` "generateCode IF"++ generadorCode body1 ++ generadorCode body2 ++ generadorCode xs `debug` "generateCode IF xs"  
    generadorCode ((Read name):xs) = [READ] ++ [STORE name] `debug` "generateCode Read" ++ generadorCode xs `debug` "generateCode Read xs"
    generadorCode ((Assig name exp):xs) = generadorExpr exp `debug` "generateCode Assig" ++ [STORE name] ++ generadorCode xs `debug` "generateCode Assig xs" 
    
    generadorExpr :: Expr -> Code
    generadorExpr (Var n) = [LOAD n] `debug` "generadorExpr Var"
    generadorExpr (BoolLit b) = if b == True then [PUSH 1] `debug` "generadorExpr BoolLit 1" else [PUSH 0] `debug` "generadorExpr BoolLit 2" 
    generadorExpr (IntLit b) = [PUSH b] `debug` "generadorExpr IntLit"
    generadorExpr (Unary Not exp) = generadorExpr exp ++ [NEG] `debug` "generadorExpr Unary" 
    generadorExpr (Binary Plus exp1 exp2) = generadorExpr exp2 ++ generadorExpr exp1 ++ [ADD] 
    generadorExpr (Binary Mod exp1 exp2) = generadorExpr exp2 ++ generadorExpr exp1 ++ [MOD]
    generadorExpr (Binary Minus exp1 exp2) = generadorExpr exp2 ++ generadorExpr exp1 ++ [SUB]
    generadorExpr (Binary Mult exp1 exp2) = generadorExpr exp2 ++ generadorExpr exp1 ++ [MUL]
    generadorExpr (Binary Div exp1 exp2) = generadorExpr exp2 ++ generadorExpr exp1 ++ [DIV]
    
    
    aux :: String -> Code
    aux a = undefined
>>>>>>> master
