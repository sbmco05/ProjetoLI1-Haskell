{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Sofia Gomes Noversa <a100603@alunos.uminho.pt>
              Sofia Beatriz Miranda Couto <a106925@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe
import Data.List
import LI12324
import Tarefa1
import Tarefa3
import Debug.Trace


{-| Atualiza o jogo com base nas ações do jogador e inimigos.
Retorna o novo jogo após aplicar as atualizações. 

== Exemplo de utilização:
>>> atualiza [(Just Subir), (Just AndarDireita)] (Just Descer) jg
Jogo {mapa = Mapa ((0.5,5.5),Oeste) (0.5,2.5) [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
,[Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio]
,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]]
, inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (1.0,2.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 2, pontos = 0, aplicaDano = (True,1.0)}
,Personagem {velocidade = (0.0,6.0), tipo = Fantasma, posicao = (1.0,1.0), direcao = Sul, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 2, pontos = 0, aplicaDano = (True,1.0)}], colecionaveis = [(Moeda,(5.0,3.0)),(Moeda,(8.0,2.0))]
, jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (3.0,2.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}}
-}

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza l a jg@(Jogo m i c j) = Jogo m (atualizaInimigos (zip l i) jg ) c (atualizaAcao a jg j)


{-| Atualiza a lista de inimigos com base nas ações e no estado atual do jogo.
Retorna a lista de inimigos atualizada.

== Exemplo de utilização:
>>> atualizaInimigos [(Just Subir, en1), (Just AndarDireita, en2)] jg 
[Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (4.5,10.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}
,Personagem {velocidade = (0.0,6.0), tipo = MacacoMalvado, posicao = (4.5,1.5), direcao = Sul, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 10, pontos = 0, aplicaDano = (False,0.0)}]
 -}

atualizaInimigos :: [(Maybe Acao, Personagem)] -> Jogo -> [Personagem]
atualizaInimigos [] _ = []
atualizaInimigos ((a, p) : r) jg = atualizaAcao a jg p : atualizaInimigos r jg
 

{-| Atualiza as ações do jogador e inimigos com base no estado atual do jogo.
 Retorna a personagem atualizada com base na ação recebida.

 == Exemplo de utilização:
 >>>atualizaAcao (Just AndarDireita) jg j2
 Personagem {velocidade = (1.0,0.0), tipo = Jogador, posicao = (1.5,2.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
 -}

atualizaAcao :: Maybe Acao -> Jogo -> Personagem -> Personagem
atualizaAcao a jg p = case a of
    Just Subir -> up jg p
    Just Descer -> down jg p
    Just AndarDireita -> andarD jg p
    Just AndarEsquerda -> andarE jg p
    Just Saltar -> saltar (mapa jg) p 
    Just Parar -> parar p (mapa jg)
    _ -> p 


{-| Verifica se a personagem está em um bloco escada.

== Exemplos de utiliação: 
>>> escada j m
False
 -}

escada :: Personagem -> Mapa -> Bool
escada p m = any (\(bloco, pos) -> bloco == Escada) (blocosColididos (blocosPosicaoMatriz m) p)

{-| Move a personagem para cima, verificando se está em uma escada (ou numa plataforma, em certos casos).
Retorna a personagem atualizada com nova posição ou estado em escada.

== Exemplos de utilização: 
>>> up jg j 
Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (3.0,2.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}

>> 
-}

up :: Jogo -> Personagem -> Personagem
up jg p 
        | escada p (mapa jg) || emEscada p = p {emEscada = True, velocidade = (0,-1), direcao = Norte}
        | (escada p (mapa jg) || emPlataforma (mapa jg) p) && blocoAbaixo (fromIntegral (ceiling x), fromIntegral (ceiling y)) (mapa jg) == Just Escada = p {velocidade = (0,-1), direcao = Norte} 
        | otherwise = parar p (mapa jg) 
    where (x,y) = posicao p
blocoAbaixo :: Posicao -> Mapa -> Maybe Bloco -- verifica qual bloco está diretamente abaixo de uma determinada posição no mapa.
blocoAbaixo (x, y) m = procuraBloco (x, y + 1.0) (blocosPosicaoMatriz m)

{-| Dada uma posição, busca um bloco em uma lista de tuplas (bloco, posição).

== Exemplo de utilização
>>> procuraBloco (2.0,3.0) [(Escada, (2.0,3.0)), (Plataforma, (3.0,4.0)), (Vazio, (1.0,9.0))]
Just Escada
-}
procuraBloco :: Posicao -> [(Bloco, Posicao)] -> Maybe Bloco
procuraBloco _ [] = Nothing
procuraBloco pos ((b, p):r)
  | pos == p = Just b
  | otherwise = procuraBloco pos r


{-| Move a personagem para baixo, verificando se está em uma escada.
Retorna a personagem atualizada com nova posição ou estado em escada.

== Exemplos de utilização:
>>> down jg j
Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (3.0,2.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
-}


down :: Jogo -> Personagem -> Personagem
down jg p 
    | escada p (mapa jg) =  p {emEscada = True, velocidade = (0,1), direcao = Sul}
    | emEscada p = p {emEscada = True, velocidade = (0,1), direcao = Sul}
    | otherwise = parar p (mapa jg)


{-| Move a personagem para a direita, verificando colisões e ressalto.
Retorna a personagem atualizada com nova posição ou estado de ressalto.

== Exemplos de utilização:
>>> andarD jg j2
Personagem {velocidade = (1.0,0.0), tipo = Jogador, posicao = (1.5,2.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
-}

andarD :: Jogo -> Personagem -> Personagem 
andarD jg@(Jogo m i _ j) p
    | ressalta p && not(emPlataforma m p) = p {velocidade = gravidade, direcao = Sul, emEscada = False}
    | not (ressalta p) && not(emPlataforma m p) && not (emEscada p) = p{velocidade = gravidade, direcao = Sul, emEscada= False}
    | ressalta p && colisaoParedeFrente m p = p {velocidade =(1, 0), direcao = Oeste, emEscada = False}
    | not (emEscada p) || (emEscada p && emPlataforma m p) = p {velocidade =(1, 0), direcao = Este, emEscada = False}
    | otherwise = parar p (mapa jg)

{- | Move a personagem para a esquerda, verificando colisões e ressalto.
Retorna a personagem atualizada com nova posição ou estado de ressalto.

== Exemplos de utilização: 
>>> andarE jg j
Personagem {velocidade = (-1.0,0.0), tipo = Jogador, posicao = (3.0,2.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
 -}
andarE :: Jogo -> Personagem -> Personagem 
andarE jg@(Jogo m i _ j) p
    | ressalta p && not(emPlataforma m p) = p {velocidade = gravidade, direcao = Sul, emEscada = False}
    | not (ressalta p) && not(emPlataforma m p) && not (emEscada p) = p {velocidade =(-1, 0), direcao = Este, emEscada = False}
    | ressalta p && colisaoParedeFrente m p = p {velocidade =(1, 0), direcao = Este, emEscada = False}
    | not (emEscada p) || (emEscada p && emPlataforma m p) = p {velocidade =(-1, 0), direcao = Oeste, emEscada = False}
    | otherwise = parar p (mapa jg)

{- | Verifica colisões de um certo inimigo com os outros inimigos.
Retorna True se a personagem colidir com algum inimigo.

== Exemplos de utilização: 
 >>> verificaColisaoInimigos jg j
 False
 -}

verificaColisaoInimigos :: Jogo -> Personagem -> Bool
verificaColisaoInimigos jg@(Jogo m [] _ _) p = False 
verificaColisaoInimigos jg@(Jogo m (i:is) _ _) p 
    | tipo p /= Jogador && p /= i = colisoesPersonagens p i  || verificaColisaoInimigos jg {inimigos = is} p
    | otherwise = verificaColisaoInimigos jg { inimigos = is} p

{- | Verifica se a personagem está em queda livre.
Retorna True se a personagem não está em uma escada e está em queda.

== Exemplos de utilização: 
>>> queda j m
False

 -}

queda :: Personagem -> Mapa -> Bool
queda p m = not (emEscada p) && not (escada p m) && velocidade p == gravidade


{-| Verifica se os personagens podem cair, ou seja, se o bloco imediatamente a seguir do bloco abaixo da personagem é vazio
Não pode estar em uma escada e não há colisão com Plataforma ou Alçapão.

== Exemplos de utilização: 
>>> verificaP m j
False
-}

verificaP :: Mapa -> Personagem -> Bool
verificaP m@(Mapa _ _ bs) p =  fst (last (blocosColididos (blocosPosicaoMatriz m) p)) /= Vazio 

{-| Faz a personagem realizar um salto.
Retorna a personagem com velocidade de salto aplicada.

 Exemplos de utilização: 
 >>> saltar m j2
Personagem {velocidade = (0.0,-40.0), tipo = Jogador, posicao = (1.5,2.5), direcao = Norte, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}

>>> saltar m j
Personagem {velocidade = (0.0,6.0), tipo = Jogador, posicao = (3.0,2.0), direcao = Sul, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
-}

saltar :: Mapa -> Personagem -> Personagem 
saltar m p@Personagem {emEscada = True} = p  
saltar m p@Personagem {velocidade = (vx, vy), emEscada = False} =
    if any (\(bloco,pos) -> (bloco == Plataforma)) (blocosColididos (blocosPosicaoMatriz m) p) then
        p { velocidade = (vx, -40), direcao = Norte} 
    else
        p { velocidade = gravidade, direcao = Sul } 
 
{-| Faz a personagem parar.
Retorna a personagem com velocidade igual a 0, exceto se estiver em queda.

== Exemplos de utilização:
>>> parar j m
Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (3.0,2.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
-}

parar :: Personagem -> Mapa -> Personagem
parar j m
    | queda j m = j
    | otherwise = j {velocidade = (0,0), emEscada = False}


