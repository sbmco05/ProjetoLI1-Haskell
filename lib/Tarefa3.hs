{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Sofia Gomes Noversa <a100603@alunos.uminho.pt>
              Sofia Beatriz Miranda Couto <a106925@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where
import LI12324
import Tarefa1
import Data.List
import Data.Maybe

{- | Atualiza o estado do jogo, movendo todos os elementos de acordo com o tempo.
Retorna um novo estado de jogo após a movimentação.

== Exemplos de utilização: 
>>>movimenta 7 1.3 jg 
Jogo {mapa = Mapa ((0.5,5.5),Oeste) (0.5,2.5) [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]]
, inimigos = [Personagem {velocidade = (1.0,0.0), tipo = Fantasma, posicao = (1.0,2.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 2, pontos = 0, aplicaDano = (True,1.0)},Personagem {velocidade = (1.0,0.0), tipo = Fantasma, posicao = (1.0,1.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 2, pontos = 0, aplicaDano = (True,1.0)}], colecionaveis = [(Moeda,(5.0,3.0)),(Moeda,(8.0,2.0))]
, jogador = Personagem {velocidade = (0.0,10.0), tipo = Jogador, posicao = (3.0,2.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}}
 -}

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta seed t jogo@(Jogo m i c j) = Jogo mapa inimigos colecionaveis jogador
                                where mapa = checkAlcapao m jogador
                                      inimigos = posInimAtual
                                      colecionaveis = col
                                      jogador = jogCol
                                      posJogAtual = deslocarJogador jog m t
                                      posInimAtual = deslocarInimigos inim jogo seed t
                                      (jog,inim) = interacaoJogInim i j
                                      (jogCol,col) = colecionaveisJog c posJogAtual

{-| Calcula a hitbox de dano de uma personagem com base na direção.
Retorna a hitbox de dano da personagem.

== Exemplos de utilização: 
>>> hitboxDano j1  
((3.5,2.5),(4.5,1.5))
-}

hitboxDano :: Personagem -> Hitbox
hitboxDano p | direcao p == Oeste = ((x1-t1,y1),(x1,y2))
             | direcao p == Este = ((x2,y1),(x2+t1,y2))
             | direcao p == Norte = ((x1,y2),(x2,y2-t2))
             | direcao p == Sul = ((x1,y1+t2),(x2,y1))
             where ((x1,y1), (x2,y2)) = hitbox p
                   (t1,t2) = tamanho p

{-| Calcula a hitbox de um colecionável com base em sua posição.

Exemplos de utilização:
>>> hitboxColecionavel (Moeda, (3.5, 1.0)) 
((3.0,1.5),(4.0,0.5))

>>> hitboxColecionavel (Martelo, (1.5, 2.0)) 
(1.0,2.5),(2.0,1.5))
 -}

hitboxColecionavel :: (Colecionavel, Posicao) -> Hitbox
hitboxColecionavel (_,(x,y)) = ((x-t1/2,y+t2/2),(x+t1/2,y-t2/2))
                              where (t1,t2) = tamanhoBlocos

{-| Desloca os inimigos com base no tempo, semente e jogo.
Retorna uma lista de inimigos com posições atualizadas.

== Exemplo de utilização: 
>>> deslocarInimigos i jg 3 2.0
[Personagem {velocidade = (1.0,0.0), tipo = Fantasma, posicao = (1.0,2.0), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 2, pontos = 0, aplicaDano = (True,1.0)}
,Personagem {velocidade = (1.0,0.0), tipo = Fantasma, posicao = (1.0,1.0), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 2, pontos = 0, aplicaDano = (True,1.0)}]  
-}

deslocarInimigos ::  [Personagem] -> Jogo -> Semente -> Tempo -> [Personagem]
deslocarInimigos [] _ _ _ = []
deslocarInimigos inimigos@(i:is) jg@(Jogo m _ _ j) s t 
      | colisoesHitbox (hitbox novoI) (hitbox j) = i {direcao = direcaoContraria (direcao i)} : deslocarInimigos is jg s t
      | not (colisaoParedeFrente m novoI) && emPlataforma m novoI = novoI : deslocarInimigos is jg s t
      | not (colisaoParedeFrente m novoI) && not(emPlataforma m novoI)= i{direcao = direcaoContraria (direcao i)} : deslocarInimigos is jg s t
      | otherwise = i{direcao = direcaoContraria (direcao i)} : deslocarInimigos is jg s t
      where (x,y) = posicao i
            novaPos = (x+dx,y+dy)
            (dx,dy) = calcDeslocamento (velocidade i) t
            novoI = i {posicao= novaPos}


{-| Inverte a direção passada como argumento

== Exemplo de utilização:
>>> direçaoContraria Norte
Sul
-}
direcaoContraria :: Direcao -> Direcao
direcaoContraria dir = case dir of 
      Norte -> Sul
      Sul -> Norte
      Este -> Oeste
      Oeste -> Este
{-| Desloca o jogador com base no tempo e mapa.
Retorna o jogador com a posição atualizada.

== Exemplo de utilização: 
>>> deslocarJogador j m 7.9 
Personagem {velocidade = (0.0,6.0), tipo = Jogador, posicao = (3.0,2.0), direcao = Sul, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}-} 

deslocarJogador :: Personagem -> Mapa -> Tempo -> Personagem
deslocarJogador jog m t | not (colisaoParedeFrente m jog) && (emPlataforma m jog || emEscada jog)= jog {posicao = novaPos, aplicaDano = atualizaDano}
                        | not (colisaoParedeFrente m jog) && not(emPlataforma m jog) = jog {posicao = novaPos,aplicaDano = atualizaDano, velocidade = gravidade, direcao = Sul}
                        | otherwise = jog{aplicaDano = atualizaDano}
                        where (x,y) = posicao jog
                              (dx,dy) = calcDeslocamento (velocidade jog) t
                              novaPos = (x+dx,y+dy)
                              (_,tempo) = aplicaDano jog
                              atualizaDano = (tempo-t > 0,max (tempo-t) 0)
                              jogNovaPos = jog {posicao = novaPos}

{-| Verifica a interação entre o jogador e os colecionáveis.
Retorna o jogador e a lista atualizada de colecionáveis.

== Exemplo de utilização: 
>>> colecionaveisJog [(Moeda,(5.0, 3.0)), (Moeda, (8.0, 2.0))]  j 
(Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (3.0,2.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)},[(Moeda,(5.0,3.0)),(Moeda,(8.0,2.0))])
-}

colecionaveisJog :: [(Colecionavel, Posicao)] -> Personagem -> (Personagem,[(Colecionavel, Posicao)])
colecionaveisJog [] j = (j,[])
colecionaveisJog (c@(col,pos):cs) j | colisoesHitbox (hitboxColecionavel c) (hitbox j) = case col of 
                                                                                          Moeda -> (j {pontos = pontos j + 10}, cs)
                                                                                          Martelo -> (j {aplicaDano = (True, 10)}, cs)
                                    | otherwise = (fst recursiva, c : snd recursiva)
                                    where recursiva = colecionaveisJog cs j

{-| Verifica a interação entre o jogador e os inimigos.
Retorna o jogador e a lista atualizada de inimigos.

== Exemplo de utilização 1: 
>>> interacaoJogInim i j 
(Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (3.0,2.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
,[Personagem {velocidade = (1.0,0.0), tipo = Fantasma, posicao = (1.0,2.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 2, pontos = 0, aplicaDano = (True,1.0)}
,Personagem {velocidade = (1.0,0.0), tipo = Fantasma, posicao = (1.0,1.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 2, pontos = 0, aplicaDano = (True,1.0)}])
-}

interacaoJogInim :: [Personagem] -> Personagem -> (Personagem, [Personagem])
interacaoJogInim i j | fst (aplicaDano j) = ((if any (\ini -> colisoesHitbox (hitbox ini) (hitbox j)) i then j{vida=max 0 (vida j-1)} else j ) , danoInimigo i j ) 
                     | otherwise = if any (\ini -> colisoesHitbox (hitbox ini) (hitbox j)) i then (j{vida= max 0 (vida j-1)}, i) else (j,i)

{-| Aplica dano aos inimigos com base na hitbox de dano do jogador.
Retorna a lista atualizada de inimigos após o dano.

== Exemplo de utilização: 
>>> danoInimigo i j 
[Personagem {velocidade = (1.0,0.0), tipo = Fantasma, posicao = (1.0,2.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 2, pontos = 0, aplicaDano = (True,1.0)}
,Personagem {velocidade = (1.0,0.0), tipo = Fantasma, posicao = (1.0,1.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 2, pontos = 0, aplicaDano = (True,1.0)}]
-}
danoInimigo :: [Personagem] -> Personagem -> [Personagem]
danoInimigo [] _ = []
danoInimigo (i:is) j | colisoesHitbox hbDanoJog hbI = i { vida = vidaI } : is
                | otherwise = i : danoInimigo is j
                  where hbDanoJog = hitboxDano j
                        hbI = hitbox i 
                        vidaa = vida i
                        vidaI = max 0 vidaa-1

{-| Verifica a existência de um alçapão abaixo do jogador e altera o mapa se encontrado.
Retorna um novo mapa após a verificação e possível alteração.

== Exemplo de utilização:
>>> checkAlcapao m j 
Mapa ((0.5,5.5),Oeste) (0.5,2.5) [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
                                 ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
                                 ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
                                 ,[Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma]
                                 ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio]
                                 ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]]
 -}
checkAlcapao :: Mapa -> Personagem -> Mapa
checkAlcapao m@(Mapa a pos b) p@(Personagem {posicao = (x,y)}) = case findAlcapao of
                                                                        Just (bloco, index) -> Mapa a pos (alteracaoMatriz b index)
                                                                        Nothing -> m
                                                                  where findAlcapao = listToMaybe $ filter (\(bloco, pos) -> bloco == Alcapao && snd pos >= y) (blocosColididos (blocosPosicaoMatriz m) p)

{-| Altera uma matriz de blocos, marcando um alçapão como Vazio.
Retorna uma matriz de blocos após a alteração.

-- Exemplo de utilização: 
>>> alteracaoMatriz [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma],[Vazio, Alcapao, Vazio],[Vazio, Vazio, Vazio]] (1.5,1.5)
[[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio]]
 -}

alteracaoMatriz :: [[Bloco]] -> Posicao -> [[Bloco]]
alteracaoMatriz matriz (x,y) = if i < length matriz && j < length (head matriz) then 
                                    take j matriz ++ [take i (matriz !! j) ++ [Vazio] ++ drop (i+1) (matriz !! j)] ++ drop (j+1) matriz
                              else 
                                    matriz
                              where (i,j)=(floor x, floor y)

{-| Calcula o deslocamento com base na velocidade e tempo.
Retorna a nova posição após o deslocamento.

== Exemplos de utilização: 
>>> calcDeslocamento (2, 1) 0.5
(1.0, 0.5)

>>> calcDeslocamento (0, -1) 1.0 
(0.0, -1.0)
 -}

calcDeslocamento :: Velocidade -> Tempo -> Posicao
calcDeslocamento (vx,vy) t = (vx*t,vy*t)

{-| Decide com base na semente se um evento aleatório deve ocorrer.
Retorna True se o evento deve ocorrer.

== Exemplo de utilização:
>>> decisao 3 
True

>>> decisao 47573 
False
-}

decisao :: Semente -> Bool
decisao s = abs (div (head(geraAleatorios s 1)) 1000000000000000000) >= 8 --(quanto mais alto, mais dificilmente dará true) <10

{-| Verifica se a personagem está em uma plataforma no mapa.

== Exemplo de utilização: 
>>> emPlataforma m j
False

>>> emPlataforma m j2
True
 -}
emPlataforma :: Mapa -> Personagem-> Bool
emPlataforma m@(Mapa _ _ b) p@(Personagem {posicao = (x,y)}) = any (\(bloco,pos) -> (bloco == Plataforma || bloco == Alcapao) && snd pos > y && fst pos == fromIntegral (floor x)) (blocosColididos (blocosPosicaoMatriz m) p)
