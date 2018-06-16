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

import MachineLang
-- se pueden agregar mas importaciones
-- en caso de ser necesario

-- configuracion de la maquina
type Conf = (Stack,Env)

-- ambiente de variables
type Env = [(Var,Integer)]
-- stack
type Stack = [Integer]

-- interprete
interp :: Code -> Code -> Conf -> IO Conf                   --interp listOut listIn (Conf stack env)
interp _ [] (sta,env) = return (sta,env)

executeCode :: Code -> Code -> Conf -> Conf
executeCode _ [] (sta,env) = (sta,env)
executeCode y ((PUSH x):xs) (sta,env) = executeCode ([PUSH x] ++ y) xs (([x] ++ sta),env)
executeCode y ((ADD):xs) (sta,env) = executeCode ([ADD] ++ y) xs ((addTwo sta),env)

addTwo :: Stack -> Stack
addTwo inSt = [(sum (take 2 inSt))] ++ (tail (tail inSt))
