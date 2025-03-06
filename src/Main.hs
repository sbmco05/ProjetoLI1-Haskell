module Main where
import Tarefa2
import LI12324
import Tarefa5
import Graphics.Gloss

main :: IO ()
main = do
  putStrLn "Hello, PrimateKong!"
  imagens <- loadImagens
  let jogoInicial = estadoInicialMenu imagens
  if valida (jogoAtual jogoInicial) then do 
    let window = InWindow "Meu Jogo" (round comprimento, round altura) (10, 10)
    play window (makeColorI 219 194 252 255) 30 (estadoInicialMenu imagens) (\estado -> desenhaJogo estado imagens) mapear reageTempo
  else
      putStrLn "Jogo inválido, impossível começar jogo." 