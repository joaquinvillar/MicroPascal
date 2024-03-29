-- LABORATORIO DE PROGRAMACION FUNCIONAL 2018
-- MODULO DEL INTERPRETE DEL LENGUAJE DE MAQUINA

-- Se debe implementar la funcion interp, que
-- dadas una lista con las instrucciones que estan
-- antes del program counter (PC), otra lista con la
-- instruccion en la que esta el PC y las posteriores,
-- y una configuracion de maquina inicial, retorna
-- la configuracion de maquina resultante de interpretar
-- el codigo. Esto se realiza en la monada IO porque
-- la interpretacion puede tener efectos de lectura y
-- escritura.

module Interpreter where

import Syntax
import MachineLang
import Debug.Trace
-- se pueden agregar mas importaciones
-- en caso de ser necesario

-- configuracion de la maquina
type Conf = (Stack,Env)

-- ambiente de variables
type Env = [(Var,Integer)]
-- stack
type Stack = [Integer]

debug = flip trace

-- interprete
interp :: Code -> Code -> Conf -> IO Conf                   --interp listOut listIn (Conf stack env)
interp y [] x = return x
interp y ((WRITE):xs) (sta,env) = do
                                    print (head sta)
                                    interp ([WRITE] ++ y) xs ((deleteList sta),env)    
interp y ((READ):xs) (sta,env) = do   
                                    val <- getLine
                                    let valInt = (read val :: Integer)
                                    interp ([READ] ++ y) xs (([valInt] ++ sta),env)                                                               
interp y ((PUSH x):xs) (sta,env) = interp ([PUSH x] ++ y) xs (([x] ++ sta),env)
interp y ((ADD):xs) (sta,env) = interp ([ADD] ++ y) xs ((addTwo sta),env) 
interp y ((NEG):xs) (sta,env) = interp ([NEG] ++ y) xs ((negTop sta),env)
interp y ((SUB):xs) (sta,env) = interp ([SUB] ++ y) xs ((subTwo sta),env)
interp y ((MUL):xs) (sta,env) = interp ([MUL] ++ y) xs ((mulTwo sta),env)
interp y ((MOD):xs) (sta,env) = interp ([MOD] ++ y) xs ((modTwo sta),env)
interp y ((CMP):xs) (sta,env) = interp ([CMP] ++ y) xs ((cmpTwo sta),env)
interp y ((JUMP x):xs) (sta,env) = if x < 0 then 
                                            interp (removeCmd (-x) y) (moveCmd (-x) y ([JUMP x] ++ xs)) ((sta),env) 
                                        else 
                                            interp (moveCmd (x) ([JUMP x] ++ xs) y) (removeCmd (x) ([JUMP x] ++ xs)) ((sta),env)
interp y ((JMPZ x):xs) (sta,env) = if (head sta) == 0 then 
                                            if x < 0 then 
                                                interp (removeCmd (-x) y) (moveCmd (-x) y ([JMPZ x] ++ xs)) ((deleteList sta),env)
                                            else 
                                                interp (moveCmd (x) ([JMPZ x] ++ xs) y) (removeCmd (x) ([JMPZ x] ++ xs)) ((deleteList sta),env)
                                        else 
                                            interp ([JMPZ x] ++ y) xs ((deleteList sta),env)
interp y ((LOAD x):xs) (sta,env) = interp ([LOAD x] ++ y) xs (([(loadEnv x env)] ++ sta),env)
interp y ((STORE x):xs) (sta,env) = interp ([STORE x] ++ y) xs ((deleteList sta),(storeEnv x (head sta) env))            
interp y ((SKIP):xs) (sta,env) = interp ([SKIP] ++ y) xs (sta,env)

addTwo :: Stack -> Stack
addTwo inSt = [(sum (take 2 inSt))] ++ (deleteList (deleteList inSt))

negTop :: Stack -> Stack
negTop neg = [- (head neg)] ++ (deleteList neg)

subTwo :: Stack -> Stack
subTwo inSt = [(head inSt) - (last (take 2 inSt))] ++ (deleteList (deleteList inSt))

mulTwo :: Stack -> Stack
mulTwo inSt = [(head inSt) * (last (take 2 inSt))] ++ (deleteList (deleteList inSt))

modTwo :: Stack -> Stack
modTwo inSt = [mod (head inSt) (last (take 2 inSt))] ++ (deleteList (deleteList inSt))

cmpTwo :: Stack -> Stack
cmpTwo inSt = do              
            if (head inSt) > (last (take 2 inSt)) 
            then [1] ++ (deleteList (deleteList inSt))
            else if (head inSt) < (last (take 2 inSt))
            then [-1] ++ (deleteList (deleteList inSt))
            else [0] ++ (deleteList (deleteList inSt)) 

moveCmd :: Shift -> Code -> Code -> Code
moveCmd 0 st std = std
moveCmd 1 st std = [(head st)] ++ std
moveCmd num st std = moveCmd (num - 1) (deleteListShi st) ([(head st)] ++ std)

removeCmd :: Shift -> Code -> Code
removeCmd 0 st = st
removeCmd 1 st = deleteListShi st
removeCmd num st = removeCmd (num - 1) (deleteListShi st)

loadEnv :: Var -> Env -> Integer
loadEnv var ((varX,intX):xs) = if var == varX then
                                    intX
                                else loadEnv var xs

storeEnv :: Var -> Integer -> Env -> Env
storeEnv var int [] = [(var,int)]
storeEnv var int ((varX,intX):xs) = if var == varX then
                                        [(varX,int)] ++ xs
                                    else [(varX,intX)] ++ (storeEnv var int xs)                             

aux :: String -> IO Conf
aux a = undefined        

deleteList :: Stack -> Stack
--deleteList [] = [] `debug` "delete list 1" 
deleteList ((x):[]) = []
deleteList ((x):xs) = xs

deleteListShi :: Code -> Code
--deleteListShi [] = [] `debug` "delete list 1" 
deleteListShi ((x):[]) = []
deleteListShi ((x):xs) = xs
