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
checkProgram (Program name defs body) = if null (verificarVarDuplidas defs) then
                                           if null (verificarUndefined body (defListaNames defs)) then
                                               (verificarTypes defs body) --`debug` "Anduvo aca Type If" 
                                           else
                                               (verificarUndefined body (defListaNames defs)) --`debug` "Anduvo aca Type Else" 
                                        else  
                                            (verificarVarDuplidas defs) -- ++ (verificarUndefined body (defListaNames defs)) 

-- Nombres duplicados
verificarVarDuplidas :: Defs -> [Error]
verificarVarDuplidas def = if null repetidos then 
                        (repetidos) --`debug` "Anduvo aca 1" 
                    else 
                        (repetidos) --`debug` "O anduvo aca 1"
                    where repetidos = [Duplicated x | x <- reverse(duplicados $ reverse(defListaNames def))]

duplicados :: [Name] -> [Name] 
duplicados [] = []
duplicados (x:xs) = if x `elem` xs then [x] ++ duplicados(xs) else duplicados(xs)

-- No definidos
verificarUndefined :: Body -> [Name] -> [Error]
verificarUndefined [] _ = []
verificarUndefined ((Read x):xs) nam = if (x `elem` nam) then verificarUndefined xs nam else [Undefined x] ++ (verificarUndefined xs nam) 
verificarUndefined ((Write exp):xs) nam = verificarUndefinedExp nam (expListaNames [exp]) ++ verificarUndefined xs nam  
verificarUndefined ((While exp body):xs) nam = verificarUndefinedExp nam (expListaNames [exp]) ++ verificarUndefined body nam ++ verificarUndefined xs nam 
verificarUndefined ((Assig n exp):xs) nam = if not (n `elem` nam) then [Undefined n] ++ verificarUndefinedExp nam (expListaNames [exp]) ++ verificarUndefined xs nam else verificarUndefinedExp nam (expListaNames [exp]) ++ verificarUndefined xs nam 
verificarUndefined ((If exp body1 body2):xs) nam = verificarUndefinedExp nam (expListaNames [exp]) ++ verificarUndefined body1 nam ++ verificarUndefined body2 nam ++ verificarUndefined xs nam  

verificarUndefinedExp :: [Name] -> [Name] -> [Error]
verificarUndefinedExp _ [] = []
verificarUndefinedExp namE (x:xs) = if not (x `elem` namE) then [Undefined x] ++ verificarUndefinedExp namE xs
                                    else verificarUndefinedExp namE xs

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

expresionType :: [Expr] -> Defs -> Type
expresionType ((Var n):xs) def = obtenerType def n 
expresionType ((Unary op exp):xs) def = if op == Not then TyBool else TyInt 
expresionType ((Binary op exp1 exp2):xs) def  = if (op == And) || (op == Equ) || (op == Or) || (op == Less) then TyBool else TyInt
expresionType ((IntLit _):xs) def = TyInt
expresionType ((BoolLit _):xs) def = TyBool

obtenerType :: Defs -> Name -> Type
obtenerType [(VarDef nam ty)] _ = ty
obtenerType ((VarDef nam ty):xs) nom = if nom == nam then ty else obtenerType xs nom


funcionType :: Defs -> Expr -> Type -> [Error]
funcionType def (Var b) tipo = if (tipo == TyInt) && (obtenerType def b) == TyBool then [Expected TyInt TyBool]  
                           else if (tipo == TyBool) && (obtenerType def b) == TyInt then [Expected TyBool TyInt]   
                           else []  

funcionType def (BoolLit b) TyInt = [Expected TyInt TyBool] 
funcionType def (IntLit b) TyBool = [Expected TyBool TyInt] 
funcionType def (Unary Not exp) TyInt = [Expected TyInt TyBool] 
funcionType def (Binary And exp1 exp2) TyInt = [Expected TyInt TyBool] 
funcionType def (Binary Or exp1 exp2) TyInt = [Expected TyInt TyBool] 
funcionType def (Binary Plus exp1 exp2) TyBool = [Expected TyBool TyInt]
funcionType def (Binary Less exp1 exp2) TyInt = [Expected TyInt TyBool]
funcionType def (Binary Div exp1 exp2) TyBool = [Expected  TyBool TyInt]
funcionType def (Binary Mod exp1 exp2) TyBool = [Expected TyBool TyInt] 
funcionType def (Binary Mult exp1 exp2) TyBool = [Expected TyBool TyInt]
funcionType def (Binary Equ exp1 exp2) TyInt =  [Expected TyInt TyBool] 
funcionType def (Binary Minus exp1 exp2) TyBool = [Expected TyBool TyInt] 
funcionType def exp tipo = [] 

-- Verifico Type
verificarTypes :: Defs -> Body -> [Error]
verificarTypes _ [] = [] 
verificarTypes def ((Write exp):xs) = verificarExpresionType exp def ++ funcionType def exp TyInt ++ verificarTypes def xs
verificarTypes def ((While exp body):xs) = verificarExpresionType exp def ++ funcionType def exp TyBool ++ verificarTypes def body ++ verificarTypes def xs
verificarTypes def ((If exp body1 body2):xs) = verificarExpresionType exp def ++ verificarTypes def body1 ++ verificarTypes def body2 ++ verificarTypes def xs

verificarTypes def ((Read nam):xs) = if obtenerType def nam == TyInt then verificarTypes def xs
                                     else [Expected TyInt (obtenerType def nam)] ++ verificarTypes def xs 
                                     

verificarTypes def ((Assig nam exp):xs) = if obtenerType def nam == TyInt then verificarExpresionType exp def ++ funcionType def exp TyInt ++ verificarTypes def xs
                                          else verificarExpresionType exp def ++ funcionType def exp TyBool ++ verificarTypes def xs
                                        
verificarExpresionType :: Expr -> Defs -> [Error]
verificarExpresionType (Var n) def = []
verificarExpresionType (IntLit int) def = [] 
verificarExpresionType (BoolLit b) def = [] 
verificarExpresionType (Unary Not exp) def =  verificarExpresionType exp def 
verificarExpresionType (Unary Neg exp) def =  verificarExpresionType exp def  
verificarExpresionType (Binary op exp1 exp2) def =     
    let undef = verificarUndefinedExp (defListaNames def) (expListaNames [exp1]) in 
        let undef1 = verificarUndefinedExp (defListaNames def) (expListaNames [exp2]) in 
    case op of
     Equ -> if null undef then 
                    if null undef1 then
                        verificarExpresionType exp1 def ++ verificarExpresionType exp2 def ++ funcionType def exp1 TyInt ++ funcionType def exp2 TyInt
                    else undef1
                else if null undef1 then  undef
                     else undef ++ undef1
     And -> if null (verificarUndefinedExp (defListaNames def) (expListaNames [exp1,exp2])) then
                verificarExpresionType exp1 def ++ verificarExpresionType exp2 def ++ funcionType def exp1 TyBool ++ funcionType def exp2 TyBool
            else verificarUndefinedExp (defListaNames def) (expListaNames [exp1,exp2]) 
     Or -> if null undef then 
                    if null undef1 then
                        verificarExpresionType exp1 def ++ verificarExpresionType exp2 def ++ funcionType def exp1 TyBool ++ funcionType def exp2 TyBool
                    else undef1 
                else if null undef1 then  undef
                     else undef ++ undef1 
     Plus -> if null undef then 
                    if null undef1 then
                        verificarExpresionType exp1 def ++ verificarExpresionType exp2 def ++ funcionType def exp1 TyInt ++ funcionType def exp2 TyInt
                    else undef1
                else if null undef1 then  undef
                     else undef ++ undef1
     Minus -> if null undef then 
                    if null undef1 then
                        verificarExpresionType exp1 def ++ verificarExpresionType exp2 def ++ funcionType def exp1 TyInt ++ funcionType def exp2 TyInt
                    else undef1
                else if null undef1 then undef
                     else undef ++ undef1
     Mult -> if null undef then 
                    if null undef1 then
                        verificarExpresionType exp1 def ++ verificarExpresionType exp2 def ++ funcionType def exp1 TyInt ++ funcionType def exp2 TyInt
                    else undef1
            else if null undef1 then undef
                     else undef ++ undef1
     Div -> if null undef then 
                    if null undef1 then
                        verificarExpresionType exp1 def ++ verificarExpresionType exp2 def  ++ funcionType def exp1 TyInt ++ funcionType def exp2 TyInt
                    else undef1
                else if null undef1 then undef
                     else undef ++ undef1
     Less -> if null undef then 
                    if null undef1 then
                       verificarExpresionType exp1 def ++ verificarExpresionType exp2 def  ++ funcionType def exp1 TyInt ++ funcionType def exp2 TyInt
                    else undef1
                else if null undef1 then  undef
                     else undef ++ undef1 
     Mod -> if null undef then 
                    if null undef1 then
                        verificarExpresionType exp1 def ++ verificarExpresionType exp2 def ++ funcionType def exp1 TyInt ++ funcionType def exp2 TyInt
                    else undef1
                else if null undef1 then undef
                     else undef ++ undef1 