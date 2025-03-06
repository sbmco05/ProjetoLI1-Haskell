{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Sofia Gomes Noversa <a100603@alunos.uminho.pt>
              Sofia Beatriz Miranda Couto <a106925@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324
 
{-| Representa o tamanho padrão dos blocos em termos de largura e altura na matriz. 
Isto é, todos os blocos representam 1 valor na matriz tanto na componente x, quanto na y.
-}
tamanhoBlocos :: (Double, Double)
tamanhoBlocos = (1,1)

{-| Calcula a Hitbox de uma personagem. A hitbox é o menor retangulo que envolve uma personagem ou objeto. 
Nesta função, é dado o canto inferior esquerdo e o canto superior direito da hitbox. 
Os valores das posições dos limites da hitbox são dados através da posicao e tamanho do personagem.

== Exemplo de utilização:

>>> hitbox (Personagem _ _ (1.0,1.0) _ (0.8,0.8) _ _ _ _ _)
((0.6,1.4),(1.4,0.6))
 -}

hitbox :: Personagem -> Hitbox
hitbox p = ((x-l/2,y+l/2),(x+l/2,y-l/2)) 
         where (x,y) = posicao p 
               (l,h) = tamanho p

{-| Calcula a Hitbox de um bloco na matriz do mapa com base na posição do bloco na matriz e no tamanho padrão dos blocos.
A hitbox é o menor retângulo que envolve uma personagem ou objeto. 
Nesta função é dado o canto inferior esquerdo e o canto superior direito da hitbox. 

== Exemplo de utilização:
>>> hitboxBloco (2.0,3.0)
((2.0,4.0),(3.0,3.0))
-}

hitboxBloco :: Posicao -> Hitbox
hitboxBloco (x,y) = ((x, y + h), (x + c, y))
            where (c,h) = tamanhoBlocos

{- | Verifica se duas Hitboxes colidem.
Retorna True se as Hitboxes colidirem, False caso contrário.

== Exemplo de utilização: 
>>> colisoesHitbox ((1,1),(2,2)) ((1.5,1.5),(2.5,2.5)) 
False
 -}

colisoesHitbox :: Hitbox -> Hitbox -> Bool
colisoesHitbox hb1 hb2 = colide hb1 hb2 || colide hb2 hb1
                         where colide :: Hitbox -> Hitbox -> Bool
                               colide ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = x1 <= x4 && x2 >= x3 && y2 <= y3 && y1 >= y4

{-| Verifica se a personagem colide com as laterais do mapa através da posicao e do tamanho do mapa.
Retorna True se houver colisão com as laterais.

== Exemplo de utilização: 
>>> colisaoLaterais m j
False
 -}

colisaoLaterais :: Mapa -> Personagem -> Bool
colisaoLaterais (Mapa _ _ matrizMapa) personagem = floor x2 > i || floor  x1 < 0 || floor y2 < 0 || floor y1 > j
                                                    where (i,j) = (length (head matrizMapa),length matrizMapa) -- (colunas,linhas) da matriz
                                                          (x,y) = posicao personagem
                                                          ((x1,y1),(x2,y2)) = hitbox personagem

{- | Obtém a lista de blocos na matriz do mapa com suas posições.
Retorna uma lista de tuplos  contendo o bloco e a posição na matriz.

== Exemplo de utilização: 
>>> blocosPosicaoMatriz (Mapa ((0.5, 0.5), Oeste) (0.5, 1.5) [[Plataforma, Plataforma, Vazio], [Plataforma, Plataforma, Vazio], [Plataforma, Plataforma, Vazio]])
[(Plataforma,(0.0,0.0)),(Plataforma,(1.0,0.0)),(Vazio,(2.0,0.0)),(Plataforma,(0.0,1.0)),(Plataforma,(1.0,1.0)),(Vazio,(2.0,1.0)),(Plataforma,(0.0,2.0)),(Plataforma,(1.0,2.0)),(Vazio,(2.0,2.0))]
 -}

blocosPosicaoMatriz :: Mapa -> [(Bloco,Posicao)]
blocosPosicaoMatriz m@(Mapa _ _ b) = concat $ zipWith (\i linha -> zipWith (\j elem -> (elem, (j, i))) [0..] linha) [0..] b

{- | Obtém a lista de blocos na matriz do mapa que colidem com o personagem.
Retorna uma lista de tuplos  contendo o bloco e a posição na matriz que colidem com a Hitbox da personagem.

== Exemplo de utilização: 
>>> blocosColididos (blocosPosicaoMatriz m) j  
[(Vazio,(2.0,1.0)),(Vazio,(3.0,1.0)),(Vazio,(2.0,2.0)),(Vazio,(3.0,2.0))]
-}

blocosColididos :: [(Bloco,Posicao)] -> Personagem -> [(Bloco,Posicao)]
blocosColididos m p = filter (\(bloco,posicao) -> colisoesHitbox (hitbox p) (hitboxBloco posicao)) m

{- | Verifica se a personagem colide com uma plataforma no mapa.
Retorna True se houver colisão com uma plataforma.

== Exemplo de utilização: 
>>> colisaoPlataforma m j 
False
-}

colisaoPlataforma :: Mapa -> Personagem -> Bool
colisaoPlataforma m@(Mapa _ _ b) p = any (\(bloco,pos) -> (bloco == Plataforma || bloco == Alcapao)) (blocosColididos (blocosPosicaoMatriz m) p)
                                                         where posP = posicao p               

{-| Verifica se a personagem colide com as laterais do mapa ou com uma plataforma.
Retorna True se houver colisão, False caso contrário.

== Exemplo de utilização: 
>>> colisoesParede m j 
False
-}

colisoesParede :: Mapa -> Personagem -> Bool 
colisoesParede m personagem  = colisaoPlataforma m personagem || colisaoLaterais m personagem

{-| Verifica se duas personagens colidem.
Retorna True se as personagens colidirem.

== Exemplo de utilização: 
colisoesPersonagens j j1
True
 -}

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = colisoesHitbox (hitbox p1) (hitbox p2)

{-| Verifica se duas personagens colidem.
Retorna True se as personagens colidirem.

== Exemplo de utilização: 
colisoesPersonagens j j1
True
 -}

colisaoParedeFrente :: Mapa -> Personagem -> Bool
colisaoParedeFrente m p 
  | emEscada p = False
  | otherwise = any (\(bp, pos) -> (bp == Plataforma || bp == Alcapao) && checkDir pos) blocos || colisaoLateraisDir m p
  where
    blocos = blocosColididos (blocosPosicaoMatriz m) p
    ((px1,py1), (px2,py2)) = hitbox p
    (px, py) = posicao p
    checkDir :: Posicao -> Bool
    checkDir (x, y) = case direcao p of
      Norte -> y < py && x == fromIntegral(floor px)
      Sul -> y > py && x == fromIntegral(floor px)
      Este -> x > px && y == fromIntegral(floor py)
      Oeste -> x < px && y == fromIntegral(floor py)

{-| Verifica se o personagem colidiu com as laterais direita do mapa.

== Exemplo de utilização:
>>> colisaoLateraisDir m j
False

--
-}
colisaoLateraisDir :: Mapa -> Personagem -> Bool
colisaoLateraisDir (Mapa _ _ matrizMapa) personagem = 
  case direcao personagem of Norte -> floor y2 < 0 || (floor x2 > i || floor x1 < 0)
                             Sul -> floor y1 >= j || (floor x2 > i || floor x1 < 0)
                             Este -> floor x2 >= i || (floor y2 < 0 || floor y1 > j)
                             Oeste -> floor x1 < 0 || (floor y2 < 0 || floor y1 > j)
  where (i,j) = (length (head matrizMapa),length matrizMapa) -- dimensões da matriz do mapa (colunas, linhas)
        ((x1,y1),(x2,y2)) = hitbox personagem

{- Estas serão as variáveis usadas no exemplos de utilização em todas as tarefas.
Exemplo de mapa:
m:: Mapa
m = Mapa ((0.5, 5.5), Oeste) (0.5, 2.5)
 [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
 ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
 ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
 ,[Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma]
 ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
 ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
 ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
 ] 

Exemplo de jogador:
j :: Personagem
j = Personagem
  { velocidade = (0, 0)
  , tipo = Jogador
  , posicao = (1.5, 2.5)
  , direcao = Este
  , tamanho = (1.0, 1.0)
  , emEscada = False
  , ressalta = False
  , vida = 3
  , pontos = 0
  , aplicaDano = (False, 0.0)
  }

Exemplo de jogo:
jg :: Jogo
jg = Jogo
  { mapa = m
  , inimigos = i
  , colecionaveis = [(Moeda,(5.0, 3.0)), (Moeda, (8.0, 2.0))] 
  , jogador = j
  }
 
Exemplo de inimigos:
i :: [Personagem]
i = [
    Personagem {
        velocidade = (1, 0),
        tipo = Fantasma,
        posicao = (1.0, 2.0),
        direcao = Este,
        tamanho = (1.0, 1.0),
        emEscada = False,
        ressalta = True,
        vida = 2,
        pontos = 0,
        aplicaDano = (True, 1.0)
    },
    Personagem {
        velocidade = (1, 0),
        tipo = Fantasma,
        posicao = (1.0, 1.0),
        direcao = Este,
        tamanho = (1.0, 1.0),
        emEscada = False,
        ressalta = True,
        vida = 2,
        pontos = 0,
        aplicaDano = (True, 1.0)
    }
    ]


Exemplo de personagem:
j1 :: Personagem
j1 = Personagem
   { velocidade = (1, 0)
   , tipo = Fantasma
   , posicao = (3.0, 2.0)
   , direcao = Este
   , tamanho = (1.0, 1.0)
   , emEscada = False
   , ressalta = True
   , vida = 3
   , pontos = 0
   , aplicaDano = (True, 1.0)
   }

Exemplo de colecionaveis:
c7 :: Colecionavel
c7 = [(Martelo, (7,4))]
c8 :: Colecionavel
c8 = [(Moeda, (2.5,7))]

Exemplo de inimigos: 
en1 :: Personagem
en1 = Personagem
  { velocidade = (0, 0)
  , tipo = Fantasma
  , posicao = (4.5,10.5)
  , direcao = Este
  , tamanho = (1.0, 1.0)
  , emEscada = False
  , ressalta = True
  , vida = 1
  , pontos = 0
  , aplicaDano = (False, 0.0)
  }

en2 :: Personagem
en2 = Personagem
  { velocidade = (0, 0)
  , tipo = MacacoMalvado
  , posicao = (4.5,1.5)
  , direcao = Este
  , tamanho = (1.0, 1.0)
  , emEscada = False
  , ressalta = True
  , vida = 10
  , pontos = 0
  , aplicaDano = (False, 0.0)
  }
  -}