echo off
echo creando test01mio.opt
MicroPascal.exe -o test/test01 > test/misalida/test01mio.opt
FC %~dp0test\salida\test01.opt %~dp0test\misalida\test01mio.opt

echo creando test01mio.cod
MicroPascal.exe -m test/test01 > test/misalida/test01mio.cod
FC %~dp0test\salida\test01.cod %~dp0test\misalida\test01mio.cod

echo creando test02mio.opt
MicroPascal.exe -o test/test02 < test/test02.in > test/misalida/test02mio.opt
FC %~dp0test\salida\test02.opt %~dp0test\misalida\test02mio.opt

echo creando test02mio.cod
MicroPascal.exe -m test/test02 < test/test02.in > test/misalida/test02mio.cod
FC %~dp0test\salida\test02.cod %~dp0test\misalida\test02mio.cod

echo creando test03mio.opt
MicroPascal.exe -o test/test03 > test/misalida/test03mio.opt
FC %~dp0test\salida\test03.opt %~dp0test\misalida\test03mio.opt

echo creando test03mio.cod
MicroPascal.exe -m test/test03 > test/misalida/test03mio.cod
FC %~dp0test\salida\test03.cod %~dp0test\misalida\test03mio.cod


echo creando test04mio.opt
MicroPascal.exe -o test/test04 > test/misalida/test04mio.opt
FC %~dp0test\salida\test04.opt %~dp0test\misalida\test04mio.opt

echo creando test04mio.cod
MicroPascal.exe -m test/test04 > test/misalida/test04mio.cod
FC %~dp0test\salida\test04.cod %~dp0test\misalida\test04mio.cod

echo creando test05mio.opt
MicroPascal.exe -o test/test05 < test/test05.in > test/misalida/test05mio.opt
FC %~dp0test\salida\test05.opt %~dp0test\misalida\test05mio.opt

echo creando test05mio.cod
MicroPascal.exe -m test/test05 < test/test05.in > test/misalida/test05mio.cod
FC %~dp0test\salida\test05.cod %~dp0test\misalida\test05mio.cod

echo creando test06mio.opt
MicroPascal.exe -o test/test06 < test/test06.in > test/misalida/test06mio.opt
FC %~dp0test\salida\test06.opt %~dp0test\misalida\test06mio.opt

echo creando test06mio.cod
MicroPascal.exe -m test/test06 < test/test06.in > test/misalida/test06mio.cod
FC %~dp0test\salida\test06.cod %~dp0test\misalida\test06mio.cod

echo creando test07mio.opt
MicroPascal.exe -o test/test07 < test/test07.in > test/misalida/test07mio.opt
FC %~dp0test\salida\test07.opt %~dp0test\misalida\test07mio.opt

echo creando test07mio.cod
MicroPascal.exe -m test/test07 < test/test07.in > test/misalida/test07mio.cod
FC %~dp0test\salida\test07.cod %~dp0test\misalida\test07mio.cod

echo creando test08mio.opt
MicroPascal.exe -o test/test08 > test/misalida/test08mio.opt
FC %~dp0test\salida\test08.opt %~dp0test\misalida\test08mio.opt

echo creando test08mio.cod
MicroPascal.exe -m test/test08 > test/misalida/test08mio.cod
FC %~dp0test\salida\test08.cod %~dp0test\misalida\test08mio.cod

echo creando test09mio.opt
MicroPascal.exe -o test/test09 > test/misalida/test09mio.opt
FC %~dp0test\salida\test09.opt %~dp0test\misalida\test09mio.opt

echo creando test09mio.cod
MicroPascal.exe -m test/test09 > test/misalida/test09mio.cod
FC %~dp0test\salida\test09.cod %~dp0test\misalida\test09mio.cod

echo creando test10mio.opt
MicroPascal.exe -o test/test10 > test/misalida/test10mio.opt
FC %~dp0test\salida\test10.opt %~dp0test\misalida\test10mio.opt

echo creando test10mio.cod
MicroPascal.exe -m test/test10 > test/misalida/test10mio.cod
FC %~dp0test\salida\test10.cod %~dp0test\misalida\test10mio.cod


echo creando test11mio.opt
MicroPascal.exe -o test/test11 > test/misalida/test11mio.opt
FC %~dp0test\salida\test11.opt %~dp0test\misalida\test11mio.opt

echo creando test11mio.cod
MicroPascal.exe -m test/test11 > test/misalida/test11mio.cod
FC %~dp0test\salida\test11.cod %~dp0test\misalida\test11mio.cod


echo creando test12mio.opt
MicroPascal.exe -o test/test12 > test/misalida/test12mio.opt
FC %~dp0test\salida\test12.opt %~dp0test\misalida\test12mio.opt

echo creando test12mio.cod
MicroPascal.exe -m test/test12 > test/misalida/test12mio.cod
FC %~dp0test\salida\test12.cod %~dp0test\misalida\test12mio.cod


echo creando test13mio.opt
MicroPascal.exe -o test/test13 > test/misalida/test13mio.opt
FC %~dp0test\salida\test13.opt %~dp0test\misalida\test13mio.opt

echo creando test13mio.cod
MicroPascal.exe -m test/test13 > test/misalida/test13mio.cod
FC %~dp0test\salida\test13.cod %~dp0test\misalida\test13mio.cod

echo creando test14mio.opt
MicroPascal.exe -o test/test14 < test/test14.in > test/misalida/test14mio.opt
FC %~dp0test\salida\test14.opt %~dp0test\misalida\test14mio.opt

echo creando test14mio.cod
MicroPascal.exe -m test/test14 < test/test14.in > test/misalida/test14mio.cod
FC %~dp0test\salida\test14.cod %~dp0test\misalida\test14mio.cod

echo creando test15mio.opt
MicroPascal.exe -o test/test15 < test/test15.in > test/misalida/test15mio.opt
FC %~dp0test\salida\test15.opt %~dp0test\misalida\test15mio.opt

echo creando test15mio.cod
MicroPascal.exe -m test/test15 < test/test15.in > test/misalida/test15mio.cod
FC %~dp0test\salida\test15.cod %~dp0test\misalida\test15mio.cod

echo creando test16mio.cod
MicroPascal.exe -o test/test16 > test/misalida/test16mio.opt
FC %~dp0test\salida\test16.opt %~dp0test\misalida\test16mio.opt

echo creando test16mio.res
MicroPascal.exe test/test16 > test/misalida/test16mio.res
FC %~dp0test\salida\test16.res %~dp0test\misalida\test16mio.res

echo creando test17mio.cod
MicroPascal.exe -o test/test17 > test/misalida/test17mio.opt
FC %~dp0test\salida\test17.opt %~dp0test\misalida\test17mio.opt

echo creando test17mio.res
MicroPascal.exe test/test17 > test/misalida/test17mio.res
FC %~dp0test\salida\test17.res %~dp0test\misalida\test17mio.res

echo creando test18mio.opt
MicroPascal.exe -o test/test18 < test/test18.in > test/misalida/test18mio.opt
FC %~dp0test\salida\test18.opt %~dp0test\misalida\test18mio.opt

echo creando test18mio.res
MicroPascal.exe test/test18 < test/test18.in > test/misalida/test18mio.res
FC %~dp0test\salida\test18.res %~dp0test\misalida\test18mio.res

echo creando test19mio.opt
MicroPascal.exe -o test/test19 < test/test19.in > test/misalida/test19mio.opt
FC %~dp0test\salida\test19.opt %~dp0test\misalida\test19mio.opt

echo creando test19mio.res
MicroPascal.exe test/test19 < test/test19.in > test/misalida/test19mio.res
FC %~dp0test\salida\test19.res %~dp0test\misalida\test19mio.res

echo creando test20mio.opt
MicroPascal.exe -o test/test20 < test/test20.in > test/misalida/test20mio.opt
FC %~dp0test\salida\test20.opt %~dp0test\misalida\test20mio.opt

echo creando test20mio.res
MicroPascal.exe test/test20 < test/test20.in > test/misalida/test20mio.res
FC %~dp0test\salida\test20.res %~dp0test\misalida\test20mio.res

echo creando test21mio.opt
MicroPascal.exe -o test/test21 > test/misalida/test21mio.opt
FC %~dp0test\salida\test21.opt %~dp0test\misalida\test21mio.opt

echo creando test21mio.res
MicroPascal.exe test/test21 > test/misalida/test21mio.res
FC %~dp0test\salida\test21.res %~dp0test\misalida\test21mio.res

echo creando test22mio.opt
MicroPascal.exe -o test/test22 > test/misalida/test22mio.opt
FC %~dp0test\salida\test22.opt %~dp0test\misalida\test22mio.opt

echo creando test22mio.res
MicroPascal.exe test/test22 > test/misalida/test22mio.res
FC %~dp0test\salida\test22.res %~dp0test\misalida\test22mio.res

echo creando test23mio.opt
MicroPascal.exe -o test/test23 < test/test23.in > test/misalida/test23mio.opt
FC %~dp0test\salida\test23.opt %~dp0test\misalida\test23mio.opt

echo creando test23mio.res
MicroPascal.exe test/test23 < test/test23.in > test/misalida/test23mio.res
FC %~dp0test\salida\test23.res %~dp0test\misalida\test23mio.res

pause
