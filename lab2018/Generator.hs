-- LABORATORIO DE PROGRAMACION FUNCIONAL 2018
-- MODULO DE GENERACION DE CODIGO DE MAQUINA


-- Se debe implementar la funcion generate, que dado
-- un AST que representa a un programa valido genera
-- el AST del codigo de maquina correspondiente

module Generator where


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
generadorCode ((If exp body1 body2):xs) = generadorExpr exp ++ createJumpZ body1 ++ createJump body2 ++ [SKIP] ++ generadorCode xs
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
generadorExpr (Binary Equ exp1 exp2) = generadorExpr exp2 ++ generadorExpr exp1 ++ [CMP] ++ [JMPZ 3] ++ [PUSH 0] ++ [JUMP 2] ++ [PUSH 1] ++ [SKIP]
generadorExpr (Binary Or exp1 exp2) = generadorExpr exp2 ++ generadorExpr exp1 ++ [JMPZ 3] ++ [JMPZ 1] ++ [PUSH 1] ++[SKIP]
generadorExpr (Binary And exp1 exp2) = generadorExpr exp2 ++ generadorExpr exp1 ++ [JMPZ 3] ++ [JMPZ 1] ++ [PUSH 1] ++ [SKIP]
generadorExpr (Binary Less exp1 exp2) = generadorExpr exp2 ++ generadorExpr exp1 ++ [CMP] ++ [PUSH 1] ++ [ADD] ++ [JMPZ 3] ++ [PUSH 0] ++ [JUMP 2] ++ [PUSH 1] ++ [SKIP]

createJumpZ :: Body -> Code
createJumpZ body = do
               let gen = generadorCode body
               (geneCodeZ gen ((length gen) + 2)) ++ gen

createJump :: Body -> Code
createJump body = do
                let gen = generadorCode body
                (geneCode gen ((length gen) + 1)) ++ gen

geneCodeZ :: [Instr] -> Int -> Code
geneCodeZ gen l = do
                [JMPZ l]    


geneCode :: [Instr] -> Int -> Code
geneCode gen l = do
                [JUMP l]              

aux :: String -> Code
aux a = undefined