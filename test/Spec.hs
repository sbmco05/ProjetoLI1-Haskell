module Main where
import Tarefa1Testes
import Tarefa2Testes
import Tarefa3Testes
import Tarefa4Testes
import Test.HUnit

main :: IO ()
main = runTestTTAndExit $ test [testesTarefa1, testesTarefa2, testesTarefa3, testesTarefa4]


{- ------- Visualizacao Mapa -----------
 0   1   2   3   4   5   6   7   8   9
----------------------------------------
|                                      |0
|                                      |1
|           |\| |\| |\| |\|            |2
|           |-|         |-|            |3
|           |-|         |-|            |4
|       |\| |\| |\| |\| |\| |\|        |5
|       |-|                 |-|        |6
|       |-|                 |-|        |7
|   |\| |\| |\| |\| |\| |\| |\| |\|    |8
|   |-|                         |-|    |9
|   |-|                         |-|    |10
|\| |\| |\| |\| |\| |\| |\| |\| |\| |\||11    -}