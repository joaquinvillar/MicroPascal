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


funcionType :: Expr -> [Error]
funcionType (BoolLit b) = [Expected TyInt TyBool] `debug` "funcionType b" 
funcionType (Unary Not exp) = [Expected TyInt TyBool] `debug` "funcionType Not" 
funcionType (Binary And exp1 exp2) = [Expected TyInt TyBool] `debug` "funcionType And" 
funcionType (Binary Or exp1 exp2) = [Expected TyInt TyBool] `debug` "funcionType Or" 
funcionType _ = [] `debug` "funcionType []" 

-- Verifico Type
verificarTypes :: Defs -> Body -> [Error]
verificarTypes _ [] = [] 
verificarTypes def ((Write exp):xs) = verificarExpresionType [exp] def TyInt ++ funcionType exp ++ verificarTypes def xs `debug` "Write" 
verificarTypes def ((While exp body):xs) = verificarExpresionType [exp] def TyBool ++ verificarTypes def body ++ verificarTypes def xs `debug` "While" 
verificarTypes def ((If exp body1 body2):xs) = verificarExpresionType [exp] def TyBool ++ verificarTypes def body1 ++ verificarTypes def body2 ++ verificarTypes def xs `debug` "If" 
verificarTypes def ((Read nam):xs) = if (nam `elem` defListaNames def) then if obtenerType def nam == TyInt then verificarTypes def xs `debug` "Read If" 
                                        else [Expected TyInt (obtenerType def nam)] ++ verificarTypes def xs `debug` "Read Else If [Expected TyInt (obtenerType def nam)]" 
                                        else [Undefined nam] ++ verificarTypes def xs 
verificarTypes def ((Assig nam exp):xs) = if not (nam `elem` defListaNames def) then [Undefined nam] ++ verificarTypes def xs `debug` "Assig Undef" 
                                        else if obtenerType def nam == TyInt then verificarExpresionType [exp] def TyInt ++ verificarTypes def xs `debug` "Assig TyInt"  
                                        else if obtenerType def nam == TyBool then verificarExpresionType [exp] def TyBool ++ verificarTypes def xs `debug` "Assig TyBool"  
                                        else [Undefined nam] ++ verificarTypes def xs 

verificarExpresionType :: [Expr] -> Defs -> Type -> [Error]
verificarExpresionType [] def  tipo = []
verificarExpresionType ((Var n):xs) def tipo = if (n `elem` defListaNames def) then if (obtenerType def n) == tipo then verificarExpresionType xs def tipo `debug` "Var IF"  else verificarExpresionType xs def tipo  `debug` "Var Else [Expected tipo (obtenerType def n)]" ++ [Expected tipo (obtenerType def n)] 
                                                else [Undefined n]
verificarExpresionType ((BoolLit b):xs) def tipo = if tipo == TyBool then verificarExpresionType xs def tipo else  verificarExpresionType xs def tipo `debug` "BoolLit Else [Expected tipo TyBool]" ++ [Expected tipo TyBool] 
verificarExpresionType ((IntLit int):xs) def tipo = if tipo == TyInt then  verificarExpresionType xs def tipo `debug` "IntLit IF" else verificarExpresionType xs def tipo `debug` "IntLit Else [Expected tipo TyInt]" ++ [Expected tipo TyInt] 
verificarExpresionType ((Unary Not exp):xs) def tipo = if tipo == TyBool then verificarExpresionType [exp] def TyBool ++ verificarExpresionType xs def tipo else verificarExpresionType xs def tipo ++ [Expected tipo TyBool]
verificarExpresionType ((Unary Neg exp):xs) def tipo = if tipo == TyInt then verificarExpresionType [exp] def TyInt ++ verificarExpresionType xs def tipo else verificarExpresionType xs def tipo ++ [Expected tipo TyInt]
verificarExpresionType ((Binary op exp1 exp2):xs) def tipo =     
    let undef = verificarUndefinedExp (defListaNames def) (expListaNames [exp1]) in 
        let undef1 = verificarUndefinedExp (defListaNames def) (expListaNames [exp2]) in 
            let expType = verificarExpresionType [exp1] def (expresionType [exp1] def) in 
                let expType1 = verificarExpresionType [exp2] def (expresionType [exp2] def) in 
    case op of
     Equ -> if null undef then 
                    if null undef1 then
                        if (length expType) > 0 then expType ++ expType1 ++ verificarExpresionType xs def tipo 
                        else if (length expType1) > 0 then expType1 ++ verificarExpresionType xs def tipo
                                else if tipo == TyBool then verificarExpresionType [exp1] def TyInt ++ verificarExpresionType [exp2] def TyInt ++ verificarExpresionType xs def tipo
                                    else verificarExpresionType [exp1,exp2] def TyInt ++ verificarExpresionType xs def tipo ++ [Expected tipo TyBool]
                    else undef1 ++ expType ++ verificarExpresionType xs def tipo
                else if null undef1 then  undef ++ expType1 ++ verificarExpresionType xs def tipo
                     else undef ++ undef1 ++ verificarExpresionType xs def tipo
     And -> if null (verificarUndefinedExp (defListaNames def) (expListaNames [exp1,exp2])) then
                if tipo == TyBool then verificarExpresionType [exp1] def TyBool ++ verificarExpresionType [exp2] def TyBool ++ verificarExpresionType xs def tipo `debug` "And If" 
                else verificarExpresionType [exp1] def TyBool ++ verificarExpresionType [exp2] def TyBool ++ verificarExpresionType xs  def tipo `debug` "And else [Expected tipo TyBool]"  ++ [Expected tipo TyBool]
            else verificarUndefinedExp (defListaNames def) (expListaNames [exp1,exp2]) ++ verificarExpresionType xs def tipo `debug` "And verificarUndefinedExp" 
     Or -> if null undef then 
                    if null undef1 then
                        if (length expType) > 0 then expType ++ expType1 ++ verificarExpresionType xs def tipo `debug` "Or length expType" 
                        else if (length expType1) > 0 then expType1 ++ verificarExpresionType xs def tipo `debug` "Or length expType1" 
                            else if tipo == TyBool then verificarExpresionType [exp1] def TyBool ++ verificarExpresionType [exp2] def TyBool ++ verificarExpresionType xs def tipo `debug` "Or If" 
                                else verificarExpresionType [exp1,exp2] def TyBool ++ verificarExpresionType xs def tipo `debug` "Or Else [Expected tipo TyBool]" ++ [Expected tipo TyBool]
                    else undef1 ++ expType ++ verificarExpresionType xs def tipo `debug` "Or else undef1" 
                else if null undef1 then  undef ++ expType1 ++ verificarExpresionType xs def tipo `debug` "Or undef1 elseIF" 
                     else undef ++ undef1 ++ verificarExpresionType xs def tipo `debug` "Or undef" 
     Plus -> if null undef then 
                    if null undef1 then
                        if (length expType) > 0 then expType ++ expType1 ++ verificarExpresionType xs def tipo `debug` "Plus length expType" 
                        else if (length expType1) > 0 then expType1 ++ verificarExpresionType xs def tipo `debug` "Plus length expType1" 
                            else if tipo == TyInt then verificarExpresionType [exp1] def TyInt ++ verificarExpresionType [exp2] def TyInt ++ verificarExpresionType xs def tipo `debug` "Plus If" 
                                else verificarExpresionType [exp1] def TyInt ++ verificarExpresionType [exp2] def TyInt ++ verificarExpresionType xs def tipo `debug` "Plus Else [Expected tipo TyInt]" ++ [Expected tipo TyInt]   
                    else undef1 ++ expType ++ verificarExpresionType xs def tipo `debug` "Plus else undef1" 
                else if null undef1 then  undef ++ expType1 ++ verificarExpresionType xs def tipo `debug` "Plus undef1 elseIF" 
                     else undef ++ undef1 ++ verificarExpresionType xs def tipo `debug` "Plus undef" 
     Minus -> if null undef then 
                    if null undef1 then
                        if (length expType) > 0 then expType ++ expType1 ++ verificarExpresionType xs def tipo 
                        else if (length expType1) > 0 then expType1 ++ verificarExpresionType xs def tipo
                            else if tipo == TyInt then verificarExpresionType [exp1] def TyInt ++ verificarExpresionType [exp2] def TyInt ++ verificarExpresionType xs def tipo
                                else verificarExpresionType [exp1,exp2] def TyInt ++ verificarExpresionType xs def tipo ++ [Expected tipo TyInt] 
                    else undef1 ++ expType ++ verificarExpresionType xs def tipo
                else if null undef1 then undef ++expType1 ++ verificarExpresionType xs def tipo
                     else undef ++ undef1 ++ verificarExpresionType xs def tipo
     Mult -> if null undef then 
                    if null undef1 then
                        if (length expType) > 0 then expType ++ expType1 ++ verificarExpresionType xs def tipo
                        else if (length expType1) > 0 then expType1 ++ verificarExpresionType xs def tipo ++[Expected tipo TyInt] 
                            else if tipo == TyInt then verificarExpresionType [exp1] def TyInt ++ verificarExpresionType [exp2] def TyInt ++ verificarExpresionType xs def tipo
                                else verificarExpresionType [exp1] def TyInt ++ verificarExpresionType [exp2] def TyInt ++ verificarExpresionType xs def tipo ++ [Expected tipo TyInt]
                    else undef1 ++ expType ++ verificarExpresionType xs def tipo
            else if null undef1 then undef ++ expType1 ++ verificarExpresionType xs def tipo
                     else undef ++ undef1 ++ verificarExpresionType xs def tipo
     Div -> if null undef then 
                    if null undef1 then
                        if (length expType) > 0 then expType ++ expType1 ++ verificarExpresionType xs def tipo
                        else if (length expType1) > 0 then expType1 ++ verificarExpresionType xs def tipo
                            else if tipo == TyInt then verificarExpresionType [exp1] def TyInt ++ verificarExpresionType [exp2] def TyInt ++ verificarExpresionType xs def tipo
                                else verificarExpresionType [exp1,exp2] def TyInt ++ verificarExpresionType xs def tipo ++ [Expected tipo TyInt]
                    else undef1 ++ expType ++ verificarExpresionType xs def tipo
                else if null undef1 then undef ++ expType1 ++ verificarExpresionType xs def tipo
                     else undef ++ undef1 ++ verificarExpresionType xs def tipo 
     Less -> if null undef then 
                    if null undef1 then
                        if (length expType) > 0 then expType ++ expType1 ++ verificarExpresionType xs def tipo
                        else if (length expType1) > 0 then expType1 ++ verificarExpresionType xs def tipo
                            else if tipo == TyBool then verificarExpresionType [exp1] def TyInt ++ verificarExpresionType [exp2] def TyInt ++ verificarExpresionType xs def tipo
                                else verificarExpresionType [exp1,exp2] def TyInt ++ verificarExpresionType xs def tipo ++ [Expected tipo TyBool] 
                    else undef1 ++ expType ++ verificarExpresionType xs def tipo
                else if null undef1 then  undef ++ expType1 ++ verificarExpresionType xs def tipo
                     else undef ++ undef1 ++ verificarExpresionType xs def tipo
     Mod -> if null undef then 
                    if null undef1 then
                        if (length expType) > 0 then expType ++ expType1 ++ verificarExpresionType xs def tipo
                        else if  (length expType1) > 0 then expType1 ++ verificarExpresionType xs def tipo
                            else if tipo == TyInt then verificarExpresionType [exp1] def TyInt ++ verificarExpresionType [exp2] def TyInt ++ verificarExpresionType xs def tipo
                                else verificarExpresionType [exp1,exp2] def TyInt ++ verificarExpresionType xs def tipo  ++ [Expected tipo TyInt] 
                    else undef1 ++ expType ++ verificarExpresionType xs def tipo
                else if null undef1 then undef ++ expType1 ++ verificarExpresionType xs def tipo
                     else undef ++ undef1 ++ verificarExpresionType xs def tipo