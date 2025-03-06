module Tarefa1Testes (testesTarefa1) where

import Test.HUnit
import Tarefa1
import LI12324

jogador1 = Personagem (0,0) Jogador (4.5, 1.5) Este (1,1) False False 10 0 (False, 0.0)

inimigo = Personagem (0.0,0.0) MacacoMalvado (5.2,1.5) Este (1,1) False True 10 0 (False, 0.0)

mapa1 = Mapa ((8.5, 6.5), Este) (5, 1.5)
    [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Alcapao, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

t11 = "Teste HitBox" ~: ((4.0,2.0),(5.0,1.0)) ~=? hitbox jogador1

t12 = "Testes Colisao Personagens" ~: True ~=? colisoesPersonagens inimigo jogador1

t13 = "Testes ColisaoParede" ~: True ~=? colisoesParede mapa1 jogador1

testesTarefa1 = test[t11, t12, t13]
