{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Sofia Gomes Noversa <a100603@alunos.uminho.pt>
              Sofia Beatriz Miranda Couto <a106925@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Data.List
import Tarefa1

{-| Função principal que valida o estado do jogo. Verifica várias condições, incluindo validação do chão, ressalto, posição, inimigos, vida, escadas, alçapões e blocos.
Retorna True se todas as verificações passarem, indicando um estado de jogo válido.
 -}

valida :: Jogo -> Bool
valida jg@(Jogo m i c j) = 
  validaChao m && validaRessalto jg && validaPosicao jg && validaInimigos jg &&
  validaVida i && validaEscadas m && validaAlcapao jg && validaBlocos jg 

{-| Verifica se a última linha do mapa é composta exclusivamente por plataformas.
Retorna True se a última linha contiver apenas plataformas, indicando um chão sólido.

== Exemplos de utilização:
>>> validaChao (Mapa (5, 3) [Plataforma] [[Plataforma, Plataforma, Plataforma], [Plataforma, Plataforma, Plataforma], [Plataforma, Plataforma, Plataforma]]) 
True
>>> validaChao (Mapa (5, 3) [Plataforma] [[Plataforma, Plataforma, Plataforma], [Vazio, Vazio, Vazio], [Plataforma, Plataforma, Plataforma]])
False 
-}

validaChao :: Mapa -> Bool
validaChao (Mapa (_, _) _ l) = length (filter (\x -> x == Plataforma) (last l)) == length (last l)

 {-| Verifica se o jogador não ressalta e se todos os inimigos ressaltam.
Retorna True se o jogador não ressalta e todos os inimigos ressaltam.

Exemplos de utilização:
>>> validaRessalto jg
False
-}
validaRessalto :: Jogo -> Bool
validaRessalto (Jogo _ i _ j) = not (ressalta j) && all ressalta i

{-| Verifica se a posição do jogador é diferente da posição de todos os inimigos.
Retorna True se a posição do jogador não colidir com nenhum inimigo.

== Exemplos de utilização: 
>>> validaPosicao j
True

 -}

validaPosicao :: Jogo -> Bool
validaPosicao (Jogo m@(Mapa p _ _) is _ j) = not (any (\ i -> colisoesHitbox (hitbox i) (hitbox j)) is)

{-| Verifica se há pelo menos dois inimigos no jogo.
Retorna True se houver pelo menos dois inimigos no jogo.

== Exemplos de utilização: 
>>> validaInimigos (Jogo m [] [(Martelo,(5, 3)), (Moeda, (8, 2))]  j)
False

>>> validaInimigos (Jogo m [Fantasma, Fantasma] [(Martelo,(5, 3)), (Moeda, (8, 2))] j) 
True 
-}

validaInimigos :: Jogo -> Bool
validaInimigos j = length (inimigos j) >= 2 


{- | Verifica se os fantasmas têm apenas uma vida.
Retorna True se todos os fantasmas têm exatamente uma vida.

== Exemplos de utilização:
>>> validaVida en1
True

>>> validaVida en3
False
 -}

validaVida :: [Personagem] -> Bool
validaVida [] = True
validaVida (p:ps)
    | tipo p == Fantasma && vida p ==1 = validaVida ps
    | tipo p /= Fantasma = validaVida ps
    | otherwise = False

{- | Verifica as condições em cada coluna do mapa, fazendo esta verificação em linha no mapa transposto.
Condições: as escadas começam ou acabam com uma plataforma e nunca com um alçapão.

== Exemplos de utilização:
>>> verificarColuna [Plataforma, Plataforma, Escada, Plataforma] 
True

>>> verificarColuna [Plataforma, Escada, Alcapao, Plataforma] 
False 
-}

verificarColuna :: [Bloco] -> Bool
verificarColuna [] = False
verificarColuna (Escada:r) = (head r == Escada || head r == Plataforma) && verificarColuna r
verificarColuna (h:Escada: r) = (h==Plataforma && head r /= Alcapao || h== Escada && (head r == Plataforma || head r == Escada))
verificarColuna _ = True


{- | Usa a função verificarColuna no mapa transposto (usando a função transpose) para verificar se as escadas estão dentro das condições estabelecidas.

== Exemplos de utilização: 
>>> validaEscadas m 
True

>>> validaEscadas (Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) [[Escada, Escada, Escada], [Vazio, Vazio, Vazio], [Plataforma, Plataforma, Plataforma]]) 
False 
-}

validaEscadas:: Mapa -> Bool
validaEscadas (Mapa (_, _) _ bs) =
  all verificarColuna (transpose bs)

{- | Verifica se a dimensão do alçapão é maior ou igual à largura do jogador.
Retorna True se a largura do jogador couber dentro das dimensões do alçapão.

Exemplos de utilização: 
>>> validaAlcapao jg
True
 -}

validaAlcapao :: Jogo -> Bool
validaAlcapao (Jogo m _ _ j) = fst tamanhoBlocos >= fst (tamanho j)

{- | Verifica se a posição do jogador coincide com um bloco de plataforma ou alçapão.
Retorna True se a posição do jogador não estiver sobre uma plataforma ou alçapão.

== Exemplos de utilização: 
>>> validaBlocos (Jogo (Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) [[Vazio, Vazio, Plataforma], [Vazio, Vazio, Vazio], [Vazio, Vazio, Vazio]]) [en1, en2] [(Martelo,(5, 3)), (Moeda, (8, 2))] j) 
True

-}

validaBlocos :: Jogo -> Bool
validaBlocos (Jogo (Mapa _ _ blocos) _ _ jogador) =
  let (x, y) = posicao jogador
  in case blocos !! floor y !! floor x of
    Plataforma -> False 
    Alcapao -> False
    _ -> True

