-- LABORATORIO DE PROGRAMACION FUNCIONAL 2018
-- MODULO DE OPTIMIZACION

-- Se debe implementar la funcion optimize, que dado
-- un AST que representa a un programa valido genera
-- un AST de un programa equivalente optimizado
-- mediante la tecnica de constant folding como se
-- especifica en la letra

module Optimizer where

import Syntax
import Debug.Trace
-- se pueden agregar mas importaciones
-- en caso de ser necesario

debug = flip trace

optimize :: Program -> Program
optimize (Program name defs body) = (Program name defs (deadCodeElim (reduceIt body))) --(Program name defs (deadCodeElim (reduceIt body)))

reduceIt :: Body -> Body
reduceIt [] = [] `debug` "reduceIt []" 
reduceIt ((Assig name expr):xs) = [Assig name (reduce expr)] ++ reduceIt xs
reduceIt ((If expr body bodyD):xs) = [If (reduce expr) (reduceIt body) (reduceIt bodyD)] `debug` "reduceIt IF"  ++ reduceIt xs `debug` "reduceIt IF xs" 
reduceIt ((While expr body):xs) = [While (reduce expr) (reduceIt body)] `debug` "reduceIt While"  ++ reduceIt xs `debug` "reduceIt While xs" 
reduceIt ((Write expr):xs) = [Write (reduce expr)] `debug` "reduceIt Write"  ++ reduceIt xs `debug` "reduceIt Write xs" 
reduceIt ((Read name):xs) = [Read name] `debug` "Read name"  ++ reduceIt xs `debug` "Read xs" 


reduce :: Expr -> Expr
reduce (Var name) = Var name
reduce (IntLit integer) = IntLit integer
reduce (BoolLit bool) = BoolLit bool
reduce (Binary Or (BoolLit x) (BoolLit y)) = if x == False && y == False then BoolLit False else BoolLit True
reduce (Binary Or (BoolLit True) _) = BoolLit True
reduce (Binary Or _ (BoolLit True)) = BoolLit True
reduce (Binary Or (BoolLit False) expr) = reduceBasic(Binary Or (BoolLit False) (reduce expr)) 
reduce (Binary Or expr (BoolLit False)) = reduceBasic(Binary Or (reduce expr) (BoolLit False)) 
reduce (Binary Or expr exprD) = reduceBasic(Binary Or (reduce expr) (reduce exprD))
reduce (Binary And (BoolLit x) (BoolLit y)) = if x == True && y == True then BoolLit True else BoolLit False
reduce (Binary And (BoolLit False) _) = BoolLit False
reduce (Binary And _ (BoolLit False)) = BoolLit False
reduce (Binary And expr (BoolLit True)) = reduceBasic(Binary And (reduce expr) (BoolLit True)) 
reduce (Binary And (BoolLit True) expr) = reduceBasic(Binary And (BoolLit True) (reduce expr)) 
reduce (Binary And exprU exprD) = reduceBasic(Binary And (reduce exprU) (reduce exprD))
reduce (Unary Not (BoolLit True)) = BoolLit False
reduce (Unary Not (BoolLit False)) = BoolLit True
reduce (Unary Not expr) = reduceBasic(Unary Not (reduce expr)) 
reduce (Unary Neg expr) = reduceBasic(Unary Neg (reduce expr)) 
reduce (Binary Equ (IntLit x) (IntLit y)) = if x == y then BoolLit True else BoolLit False
reduce (Binary Equ exp expd) = reduceBasic(Binary Equ (reduce exp) (reduce expd)) 
reduce (Binary Less (IntLit x) (IntLit y)) = if x < y then BoolLit True else BoolLit False
reduce (Binary Less exp expd) = reduceBasic(Binary Less (reduce exp) (reduce expd)) 
reduce (Binary Plus (IntLit x) (IntLit y)) = IntLit (x + y)
reduce (Binary Plus exp expd) = reduceBasic(Binary Plus (reduce exp) (reduce expd)) 
reduce (Binary Minus (IntLit x) (IntLit y)) = IntLit (x - y)
reduce (Binary Minus exp expd) = reduceBasic(Binary Minus (reduce exp) (reduce expd)) 
reduce (Binary Mult x (IntLit 0)) = IntLit 0
reduce (Binary Mult (IntLit 0) y) = IntLit 0
reduce (Binary Mult x (IntLit 1)) = x
reduce (Binary Mult (IntLit 1) y) = y
reduce (Binary Mult (IntLit x) (IntLit y)) = IntLit (x * y)
reduce (Binary Mult exp expd) = reduceBasic(Binary Mult (reduce exp) (reduce expd)) 
reduce (Binary Div (IntLit x) (IntLit y)) = IntLit (div x y)
reduce (Binary Div exp expd) = reduceBasic(Binary Div (reduce exp) (reduce expd)) 
reduce (Binary Mod (IntLit x) (IntLit y)) = IntLit (mod x y)
reduce (Binary Mod exp expd) = reduceBasic(Binary Mod (reduce exp) (reduce expd)) 
--faltan casos que no son enteros que son var

reduceBasic :: Expr -> Expr
reduceBasic (Var name) = Var name
reduceBasic (IntLit integer) = IntLit integer
reduceBasic (BoolLit bool) = BoolLit bool
reduceBasic (Binary Or (BoolLit x) (BoolLit y)) = if x == False && y == False then BoolLit False else BoolLit True
reduceBasic (Binary Or (BoolLit True) _) = BoolLit True
reduceBasic (Binary Or _ (BoolLit True)) = BoolLit True
reduceBasic (Binary And (BoolLit x) (BoolLit y)) = if x == True && y == True then BoolLit True else BoolLit False
reduceBasic (Binary And (BoolLit False) _) = BoolLit False
reduceBasic (Binary And _ (BoolLit False)) = BoolLit False
reduceBasic (Unary Not (BoolLit True)) = BoolLit False
reduceBasic (Unary Not (BoolLit False)) = BoolLit True
reduceBasic (Binary Equ (IntLit x) (IntLit y)) = if x == y then BoolLit True else BoolLit False
reduceBasic (Binary Less (IntLit x) (IntLit y)) = if x < y then BoolLit True else BoolLit False
reduceBasic (Binary Plus (IntLit x) (IntLit y)) = IntLit (x + y)
reduceBasic (Binary Minus (IntLit x) (IntLit y)) = IntLit (x - y)
reduceBasic (Binary Mult x (IntLit 0)) = IntLit 0
reduceBasic (Binary Mult (IntLit 0) y) = IntLit 0
reduceBasic (Binary Mult x (IntLit 1)) = x
reduceBasic (Binary Mult (IntLit 1) y) = y
reduceBasic (Binary Mult (IntLit x) (IntLit y)) = IntLit (x * y)
reduceBasic (Binary Div (IntLit x) (IntLit y)) = IntLit (div x y)
reduceBasic (Binary Mod (IntLit x) (IntLit y)) = IntLit (mod x y)
reduceBasic x = x 


deadCodeElim :: Body -> Body
deadCodeElim [] = []
deadCodeElim ((While (BoolLit False) body):xs) = deadCodeElim xs
deadCodeElim ((While exp body):xs) = [While exp (deadCodeElim body)] ++ deadCodeElim xs
deadCodeElim ((If (BoolLit False) body bodyD):xs) = (deadCodeElim bodyD) ++ deadCodeElim xs
deadCodeElim ((If (BoolLit True) body bodyD):xs) = (deadCodeElim body) ++ deadCodeElim xs 
deadCodeElim ((If exp body bodyD):xs) = [If exp (deadCodeElim body) (deadCodeElim bodyD)] ++ deadCodeElim xs 
deadCodeElim ((x):xs) = [x] ++ deadCodeElim xs
