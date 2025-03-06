{-|
Module      : Tarefa5
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Sofia Gomes Noversa <a100603@alunos.uminho.pt>
              Sofia Beatriz Miranda Couto <a106925@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 de LI1 em 2023/24.
-}
module Tarefa5 where
import Data.Maybe
import Graphics.Gloss
import LI12324
import Tarefa1
import Tarefa2
import Tarefa4
import Tarefa3
import Graphics.Gloss.Interface.IO.Interact


data Estado = Estado 
    {   jogoAtual :: Jogo,
        imagens :: Imagens,
        modo :: Modo,
        jogoClean :: Jogo,
        acaoJog :: Maybe Acao,
        menu :: MenuInicial,
        opcao :: Opcoes,
        nivel :: Int
    }

data Imagem
  = BlocoPlataforma
  | BlocoAlcapao
  | PersJogador
  | PersFantasma
  | PersMacacoMalvado
  | ColMoeda
  | ColMartelo
  | Estrela
  | BlocoEscada
  | MJ
  | Escolher
  | AutoriaM
  | MenuBG
  | DesertoM 
  | LagoaM
  | IlhasM
  | PortalM
  | Autoras
  | Voltar
  | VidaF
  | DesertoBG
  | LagoaBG
  | IlhasBG
  | PortalBG
  | JogadorArmado
  | GameOver
  | Victory
  | Nome

  deriving (Show, Eq)

type Imagens = [(Imagem, Picture)]

data MenuInicial = MenuPrincipal | EscolherNivelMenu | PlayerMenu  | CreditosMenu deriving (Show, Eq) -----------------------------------------------------------------
 
data Opcoes 
  = Jogar 
  | EscolherNivel 
  | HistoricoPlayers  
  | Creditos 
  | Mapa1 
  | Mapa2 
  | Mapa3 
  | Mapa4 
  | VoltarMenuIncial 
  
  deriving (Show, Eq)

data Modo = MenuInicial | EmJogo | Vitoria | Derrota deriving (Show, Eq)

type PontosJogador = [(String, Int)] 

{-| Retorna a altura da tela do jogo.
-}
altura :: Float
altura = 1000

{-| Retorna o comprimento da tela do jogo.
-}
comprimento :: Float
comprimento = 900

{-|Recebe um mapa e retorna um par representando as dimensões da matriz do mapa.
-}
tamanhoMatriz :: Mapa -> (Int,Int)
tamanhoMatriz (Mapa _ _ b) = (length (head b),length b)

{-| Recebe um mapa e retorna a largura de um bloco no jogo.
-}
larguraBloco :: Mapa -> Double
larguraBloco m = realToFrac comprimento / fromIntegral(fst (tamanhoMatriz m))

{-| Recebe um mapa e retorna a altura de um bloco no jogo
-}
alturaBloco :: Mapa -> Double
alturaBloco m = realToFrac altura / fromIntegral(snd (tamanhoMatriz m))

{-| Carrega imagens em formato BMP, utilizando a função loadBMP para carregar cada imagem associada a uma determinada chave (identificada pelos construtores de dados)-}
loadImagens :: IO Imagens
loadImagens = do
  menubg <- loadBMP "Imagens/menubg.bmp"
  desertobg <- loadBMP "Imagens/desertobg.bmp"
  lagoabg <- loadBMP "Imagens/lagoabg.bmp"
  ilhasbg <- loadBMP "Imagens/ilhasbg.bmp"
  portalbg <- loadBMP "Imagens/portalbg.bmp"
  menujogar <- loadBMP "Imagens/menujg.bmp"
  escolher <- loadBMP "Imagens/niveis.bmp"
  creditos <- loadBMP "Imagens/creditos.bmp"
  voltar <- loadBMP "Imagens/voltar.bmp"
  vidaf <- loadBMP "Imagens/fundovida.bmp"
  deserto <- loadBMP "Imagens/deserto.bmp"
  lagoa <- loadBMP "Imagens/lagoa.bmp"
  ilhas <- loadBMP "Imagens/ilhas.bmp"
  portal <- loadBMP "Imagens/portal.bmp"
  autoria <- loadBMP "Imagens/autoria.bmp"
  plataforma <- loadBMP "Imagens/plataforma1.bmp"
  alcapao <- loadBMP "Imagens/alçapao1.bmp"
  jogador <- loadBMP "Imagens/jogD.bmp"
  jogadorArmado <- loadBMP "Imagens/jogEarma.bmp"
  fantasma <- loadBMP "Imagens/mumiaD.bmp"
  macacoMalvado <- loadBMP "Imagens/diaboD.bmp"
  moeda <- loadBMP "Imagens/poçao.bmp"
  martelo <- loadBMP "Imagens/arma.bmp"
  estrela <- loadBMP "Imagens/chave.bmp"
  escada <- loadBMP "Imagens/escada1.bmp"
  over <- loadBMP "Imagens/over.bmp"
  victory <- loadBMP "Imagens/victory.bmp"
  nome <- loadBMP "Imagens/nome.bmp"
  let imgs = [(BlocoPlataforma, plataforma), (BlocoAlcapao, alcapao), (PersJogador, jogador), (PersFantasma,fantasma), 
              (PersMacacoMalvado, macacoMalvado), (ColMoeda, moeda), (ColMartelo, martelo), 
              (BlocoEscada,escada), (MJ, menujogar), (AutoriaM, creditos),(Escolher, escolher),
              (Voltar, voltar), (DesertoM, deserto), (LagoaM, lagoa), (IlhasM, ilhas),(PortalM, portal), 
              (Autoras, autoria), (MenuBG, menubg), (Estrela, estrela), (VidaF, vidaf), 
              (DesertoBG, desertobg), (LagoaBG, lagoabg),(IlhasBG, ilhasbg),(PortalBG, portalbg), (JogadorArmado, jogadorArmado),
              (GameOver, over), (Victory, victory), (Nome, nome)]
  return imgs

-- Constante que representa uma imagem vazia
vazio :: Picture
vazio = Blank

{-| Recebe uma Imagem e um dicionário de Imagens, onde uma Imagem é mapeada para uma Picture-}
getImagem :: Imagem -> Imagens -> Picture
getImagem i ps = fromJust (lookup i ps) 

{-|  Recebe um Bloco e um dicionário de Imagens. Com base no tipo de Bloco, ela utiliza a função getImagem para obter a Picture correspondente a esse tipo no dicionário de imagens.
-}
desenhaBloco :: Bloco -> Imagens -> Picture
desenhaBloco bloco imgs = case bloco of
                        Plataforma -> getImagem BlocoPlataforma imgs
                        Escada     -> getImagem BlocoEscada imgs
                        Alcapao    -> getImagem BlocoAlcapao imgs
                        Vazio      -> vazio

{-|  Utiliza a função blocosPosicaoMatriz para obter uma lista de tuplas contendo o tipo de bloco e sua posição na matriz do mapa. 
Para cada tupla, a função cria uma Picture correspondente ao bloco, utilizando a função desenhaBloco e usa a função pictures para compor todas essas imagens juntas.
Cada Picture é criada com base nas coordenadas do bloco na tela (translate) e na escala definida para a largura e altura do bloco (scale). 
A escala é ajustada para garantir que as imagens tenham um tamanho proporcional ao tamanho padrão (50x50) esperado para os blocos.
-}
desenhaMapa :: Mapa -> Imagens -> Picture
desenhaMapa m imgs = 
    pictures[translate (comprimento/(-2) + realToFrac x*realToFrac (larguraBloco m) + realToFrac (larguraBloco m)/2)
        (altura/2 - realToFrac y*realToFrac (alturaBloco m)-realToFrac (alturaBloco m)/2) 
        (scale (realToFrac(larguraBloco m )/ 50) (realToFrac(alturaBloco m) /50) $ desenhaBloco bloco imgs)
            | (bloco,(x,y)) <- blocosPosicaoMatriz m
            ]

{-| Desenha o jogador na tela. 
Se o jogador está morto, retorna uma imagem em branco (Blank). -}
desenhaJogador :: Estado -> Imagens -> Picture
desenhaJogador e imgs | vida (jogador jog) > 0 = if direcao jd == Oeste then 
        translate ex ey (scale ((realToFrac(larguraBloco (mapa jog)*tx)/ 50)*(-1)) 
        (realToFrac(alturaBloco (mapa jog)*ty)/50) $ if fst (aplicaDano jd) then getImagem JogadorArmado imgs else getImagem PersJogador imgs)
        else 
        translate ex ey (scale ((realToFrac(larguraBloco (mapa jog)*tx)/ 50)) 
        (realToFrac(alturaBloco (mapa jog)*ty)/50) $ if fst (aplicaDano jd) then getImagem JogadorArmado imgs else getImagem PersJogador imgs)
                        | otherwise = Blank
                        where (x,y) = posicao (jogador jog)
                              (tx,ty) = tamanho (jogador jog)
                              m = mapa jog
                              (ex,ey) = posicaoEcra m (x,y)
                              jd = jogador jog
                              jog = jogoAtual e

{-| Desenha os colecionáveis na tela.
Se a lista de colecionáveis no jogo for vazia, ela retorna uma imagem em branco (Blank). 
Senão, cria uma lista de imagens, cada uma representando um colecionável, e posiciona-as de acordo com suas coordenadas na matriz do jogo. 
-}
desenhaColecionaveis :: Jogo -> Imagens -> Picture
desenhaColecionaveis (Jogo _ _ [] _) _ = Blank
desenhaColecionaveis jg@(Jogo m _ c _) imgs = pictures[ translate ((-comprimento)/2 + realToFrac x * realToFrac (larguraBloco m) +
                                realToFrac (larguraBloco m)/2)(altura/2 - realToFrac y* realToFrac (alturaBloco m)-realToFrac (alturaBloco m)/2) 
                                (scale (realToFrac (larguraBloco m) / 50) (realToFrac(alturaBloco m) /50) $ desenhaColecionavel colecionavel imgs)
                                |colecionavel@(col, (x,y)) <- c
                                ]
{-| Seleciona a imagem correspondente ao tipo de colecionável (Martelo ou Moeda) e dimensiona-na.
-}
desenhaColecionavel :: (Colecionavel, Posicao) -> Imagens -> Picture
desenhaColecionavel (col,pos) imgs = case col of 
              Martelo -> scale 0.5 0.5 (getImagem ColMartelo imgs)
              Moeda   -> scale 0.5 0.5 (getImagem ColMoeda imgs)

{-| Desenha todos os inimigos presentes no jogo. 
Se não houver inimigos, retorna uma imagem em branco (Blank). 
Caso contrário,utiliza a função  desenhaInimigo para desenhar cada inimigo individualmente.
-}
desenhaInimigos :: Jogo -> Imagens -> Picture
desenhaInimigos jog@(Jogo _ [] _ _  ) _ = Blank
desenhaInimigos jog@(Jogo _ inimigos _ _  ) imgs = pictures
  [translate ((-comprimento)/2 + realToFrac x * realToFrac (larguraBloco m))
              (altura/2 - realToFrac y * realToFrac (alturaBloco m))
              $ desenhaInimigo inimigo jog imgs
  | inimigo@(Personagem {posicao = (x, y)}) <- inimigos
  ]
  where m = mapa jog

{-| Devolver imagem que representa o inimigo na posição correta do jogo, com escala e direção ajustadas.  
Se a vida do inimigo (vida i) for maior que 0, decide qual tipo de inimigo é (Fantasma ou Macaco Malvado) e ajusta a escala e a direção.
Caso contrário, retorna uma imagem em branco (BlanvalidaEscadas (Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) [[Escada, Escada, Escada], [Vazio, Vazio, Vazio], [Plataforma, Plataforma, Plataforma]]) k), indicando que o inimigo está morto.
-}
desenhaInimigo :: Personagem -> Jogo -> Imagens -> Picture
desenhaInimigo i jog imgs | vida i > 0 = case tipo i of 
  Fantasma -> if direcao i == Oeste then scale ((realToFrac(larguraBloco (mapa jog) * tx)/ 50)*(-1)) (realToFrac(alturaBloco (mapa jog) * ty)/50) (getImagem PersFantasma imgs)
              else scale (realToFrac(larguraBloco (mapa jog) * tx)/ 50) (realToFrac(alturaBloco (mapa jog) * ty)/50) (getImagem PersFantasma imgs)
  _ -> if direcao i == Oeste then scale ((realToFrac(larguraBloco (mapa jog) * tx)/ 50)*(-1)) (realToFrac(alturaBloco (mapa jog) * ty)/50) (getImagem PersMacacoMalvado imgs)
              else scale (realToFrac(larguraBloco (mapa jog) * tx)/ 50) (realToFrac(alturaBloco (mapa jog) * ty)/50) (getImagem PersMacacoMalvado imgs)
                          | otherwise = Blank
  where (tx,ty) = tamanho i

{-|  Dependendo do modo de jogo no estado, decide o que desenhar na tela. 
Se o modo for "EmJogo", desenha vários elementos do jogo. 
Caso contrário, desenha o menu principal. 
-}
desenhaJogo :: Estado -> Imagens -> Picture
desenhaJogo e imgs = case modo e of 
                      EmJogo -> pictures [ desenhaFundo e imgs
                                        , desenhaMapa (mapa j) imgs
                                        , desenhaJogador e imgs
                                        , desenhaColecionaveis j imgs
                                        , desenhaInimigos j imgs
                                        , desenhaInfo (jogador j) imgs
                                        , desenhaEstrela (mapa j) imgs
                                        ]
                      MenuInicial -> desenhaMenuPrincipal e
                      Vitoria -> desenharVitoria imgs
                      Derrota -> desenharDerrota imgs
                   where j = jogoAtual e

{-| Cria uma imagem composta por uma imagem da barra de vida e por um texto que exibe informações sobre o jogador, como pontuação e número de vidas.
-}
desenhaInfo :: Personagem -> Imagens -> Picture
desenhaInfo jog imgs = pictures [
    translate 190 (altura/2-50) $ scale (comprimento/750) (altura/1660)  $ getImagem VidaF imgs,
    translate 0 (altura/2-50) $ scale 0.2 0.2 $ text ("Jogador: " ++ show (pontos jog) ++ " pontos |" ++ show (vida jog) ++" vidas")]

{-| Desenha a estrela (chave) na tela
-}
desenhaEstrela :: Mapa -> Imagens ->  Picture
desenhaEstrela m@(Mapa _ (tx,ty) _) imgs = translate (fst (posicaoEcra m (tx,ty))) (snd (posicaoEcra m (tx,ty))) $ getImagem Estrela imgs

{-| Desenhar a caixa de vitória na tela.
-}
desenharVitoria :: Imagens -> Picture
desenharVitoria imgs = pictures [ translate 0 0 $ scale 1 1 $ (getImagem Victory imgs), translate (-250) (-250) $ scale 0.2 0.2 $ text "(Pressiona space para voltar ao Menu)"]

{-| Desenhar a caixa de derrota na tela.
-}
desenharDerrota :: Imagens -> Picture
desenharDerrota imgs = pictures[ translate 0 0 $ scale 1 1 $ (getImagem GameOver imgs), translate (-250) (-250) $ scale 0.2 0.2 $ text "(Pressiona space para tentar outra vez)"]

{-| Converte uma posição no mapa para as coordenadas correspondentes na tela. 
-}
posicaoEcra :: Mapa -> Posicao -> (Float,Float)
posicaoEcra m (x,y) = ((-comprimento)/2 + realToFrac x*realToFrac (larguraBloco m), altura/2 - realToFrac y*realToFrac (alturaBloco m))

{-| Desenha os elementos do menu principal com base no estado do jogo.
A opção selecionada é destacada com um tamanho maior.
-}
desenhaMenuPrincipal :: Estado -> Picture
desenhaMenuPrincipal e = case menu e of
  MenuPrincipal -> pictures [
    fundoMenu,
    translate 0 (altura/2 - 3*realToFrac (alturaBloco (mapa(jogoAtual e))) ) $ scale 1.2 1.2 $ (getImagem Nome imgs),
    translate 0 (altura/2 - 6*realToFrac (alturaBloco (mapa(jogoAtual e))) ) $ if opcao e == Jogar then scale 1.40 1.40 (getImagem MJ imgs) else scale 1.20 1.20 (getImagem MJ imgs),
    translate 0 (altura/2 - 8*realToFrac (alturaBloco (mapa(jogoAtual e))) ) $ if opcao e == EscolherNivel then scale 1.40 1.40 (getImagem Escolher imgs) else scale 1.20 1.20 (getImagem Escolher imgs),
    translate 0 (altura/2 - 10*realToFrac (alturaBloco (mapa(jogoAtual e))) ) $ if opcao e == Creditos then scale 1.40 1.40 (getImagem AutoriaM imgs) else scale 1.20 1.20 (getImagem AutoriaM imgs)         
    ]                    
  EscolherNivelMenu -> pictures [
    fundoMenu,
    translate 0 (altura/2 - 3*realToFrac (alturaBloco (mapa(jogoAtual e))) ) $ if opcao e == Mapa1 then scale 1.40 1.40 (getImagem LagoaM imgs) else scale 1.20 1.20 (getImagem LagoaM imgs),
    translate 0 (altura/2 - 5*realToFrac (alturaBloco (mapa(jogoAtual e))) ) $ if opcao e == Mapa2 then scale 1.40 1.40 (getImagem DesertoM imgs) else scale 1.20 1.20 (getImagem DesertoM imgs),
    translate 0 (altura/2 - 7*realToFrac (alturaBloco (mapa(jogoAtual e))) ) $ if opcao e == Mapa3 then scale 1.40 1.40 (getImagem IlhasM imgs) else scale 1.20 1.20 (getImagem IlhasM imgs),
    translate 0 (altura/2 - 9*realToFrac (alturaBloco (mapa(jogoAtual e))) ) $ if opcao e == Mapa4 then scale 1.40 1.40 (getImagem PortalM imgs) else scale 1.20 1.20 (getImagem PortalM imgs),
    translate 0 (altura/2 - 11*realToFrac (alturaBloco (mapa(jogoAtual e))) ) $ if opcao e == VoltarMenuIncial then scale 1.40 1.40 (getImagem Voltar imgs) else scale 1.20 1.20 (getImagem Voltar imgs)         
    ]   
  CreditosMenu -> pictures [
    fundoMenu,
    translate 0 (altura/2 - 5*realToFrac (alturaBloco (mapa(jogoAtual e))) ) $  scale 1.40 1.40 (getImagem Autoras imgs),         
    translate 0 (altura/2 - 11*realToFrac (alturaBloco (mapa(jogoAtual e))) ) $ if opcao e == VoltarMenuIncial then scale 1.40 1.40 (getImagem Voltar imgs) else scale 1.20 1.20 (getImagem Voltar imgs)         
    ] 
  where
    imgs = imagens e
    fundoMenu = translate (0) (0) $ scale (comprimento/740) (altura/740)  $ getImagem MenuBG imgs

{-| Desenha o fundo do jogo com base no nível atual.
-}
desenhaFundo :: Estado -> Imagens -> Picture 
desenhaFundo e imgs = case nivel e of
  1 -> translate (0) (0) $ scale (comprimento/740) (altura/740)  $ getImagem LagoaBG imgs
  2 -> translate (0) (0) $ scale (comprimento/740) (altura/740)  $ getImagem DesertoBG imgs
  3 -> translate (0) (0) $ scale (comprimento/740) (altura/740)  $ getImagem IlhasBG imgs
  4 -> translate (0) (0) $ scale (comprimento/740) (altura/740)  $ getImagem PortalBG imgs

{-| Mapeamento de eventos para ações no jogo.
-}
mapear:: Event -> Estado -> Estado
--Mapeamento de Eventos para o Jogador.
mapear(EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = EmJogo} = e{acaoJog = Just Subir}
mapear(EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = EmJogo} = e{acaoJog = Just Descer}
mapear(EventKey (SpecialKey KeyRight) Down _ _) e@Estado {modo = EmJogo} = e{acaoJog = Just AndarDireita}
mapear(EventKey (SpecialKey KeyLeft) Down _ _) e@Estado {modo = EmJogo} = e{acaoJog = Just AndarEsquerda}
mapear(EventKey (SpecialKey KeySpace) Down _ _) e@Estado {modo = EmJogo}= e{acaoJog = Just Saltar} 
mapear(EventKey (SpecialKey _) Up _ _) e@Estado {modo = EmJogo} = e{acaoJog = Just Parar}

-- Mapeamento de Eventos no Menu:
mapear(EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = MenuInicial, menu = MenuPrincipal, opcao = Jogar} = e{opcao = Creditos}
mapear(EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = MenuInicial, menu = MenuPrincipal, opcao = EscolherNivel} = e{opcao = Jogar}
mapear(EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = MenuInicial, menu = MenuPrincipal, opcao = Creditos} = e{opcao = EscolherNivel}

mapear(EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = MenuInicial, menu = MenuPrincipal, opcao = Jogar} = e{opcao = EscolherNivel}
mapear(EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = MenuInicial, menu = MenuPrincipal, opcao = EscolherNivel} = e{opcao = Creditos}
mapear(EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = MenuInicial, menu = MenuPrincipal, opcao = Creditos} = e{opcao = Jogar}

mapear(EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial, menu = MenuPrincipal, opcao = Jogar} = e{modo = EmJogo, acaoJog = Nothing}
mapear(EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial, menu = MenuPrincipal, opcao = EscolherNivel} = e{menu = EscolherNivelMenu, opcao = Mapa1}
mapear(EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial, menu = MenuPrincipal, opcao = Creditos} = e{menu = CreditosMenu, opcao = VoltarMenuIncial}

mapear(EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = MenuInicial, menu = EscolherNivelMenu, opcao = Mapa1} = e{opcao = Mapa2}

mapear(EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = MenuInicial, menu = EscolherNivelMenu, opcao = Mapa2} = e{opcao = Mapa3}
mapear(EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = MenuInicial, menu = EscolherNivelMenu, opcao = Mapa3} = e{opcao = Mapa4}
mapear(EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = MenuInicial, menu = EscolherNivelMenu, opcao = Mapa4} = e{opcao = VoltarMenuIncial}
mapear(EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = MenuInicial, menu = EscolherNivelMenu, opcao = VoltarMenuIncial} = e{opcao = Mapa1}

mapear(EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = MenuInicial, menu = EscolherNivelMenu, opcao = Mapa1} = e{opcao = VoltarMenuIncial}
mapear(EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = MenuInicial, menu = EscolherNivelMenu, opcao = VoltarMenuIncial} = e{opcao = Mapa4}
mapear(EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = MenuInicial, menu = EscolherNivelMenu, opcao = Mapa4} = e{opcao = Mapa3}
mapear(EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = MenuInicial, menu = EscolherNivelMenu, opcao = Mapa3} = e{opcao = Mapa2}
mapear(EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = MenuInicial, menu = EscolherNivelMenu, opcao = Mapa2} = e{opcao = Mapa1}

mapear(EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial, menu = EscolherNivelMenu, opcao = Mapa1} = iniciarJogoNoNivel e 1
mapear (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial, menu = EscolherNivelMenu, opcao = Mapa2} = iniciarJogoNoNivel e 2
mapear (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial, menu = EscolherNivelMenu, opcao = Mapa3} = iniciarJogoNoNivel e 3
mapear (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial, menu = EscolherNivelMenu, opcao = Mapa4} = iniciarJogoNoNivel e 4

mapear(EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial, opcao = VoltarMenuIncial} = e{modo = MenuInicial, menu = MenuPrincipal, opcao = Jogar}
mapear(EventKey (SpecialKey KeySpace) Down _ _) e@Estado {modo = Derrota} = e{modo = EmJogo, jogoAtual = jogoClean e}
mapear(EventKey (SpecialKey KeySpace) Down _ _) e@Estado {modo = Vitoria} = e{modo = MenuInicial, menu = MenuPrincipal, opcao = Jogar}

--Caso padrão:
mapear _ e = e{acaoJog = Nothing}

{-| Reage ao tempo no estado do jogo durante o modo "EmJogo".
-}
reageTempo :: Float -> Estado -> Estado
reageTempo t e@Estado {modo = EmJogo, jogoAtual = ja, acaoJog = acaoJogador, nivel = nivelAtual}
    | vida (jogador ja) == 0  = e{modo = Derrota}  
    | chegadaEstrela (jogador ja) (mapa ja) = estadoInicial (imagens e) (e { jogoAtual = proximoJogo, nivel = nivelAtual + 1 })
    | otherwise = e { jogoAtual = movimenta 20 (realToFrac t) jogoVelocidadesAtualizadas }
  where
    jogoVelocidadesAtualizadas = atualiza (acoes (inimigos ja)) acaoJogador ja
    proximoJogo = case nivelAtual + 1 of
      2 -> jogo2
      3 -> jogo3
      4 -> jogo4
      _ -> jogoInicial
reageTempo _ e = e

{-| Verifica se o jogador atingiu a estrela num determinado mapa.
-}
chegadaEstrela :: Personagem -> Mapa -> Bool
chegadaEstrela jog (Mapa _ (x,y)_) = colisoesHitbox (hitbox jog) (hitboxBloco (x,y)) 

{-| Recebe um Estado e um número de nível escolhido. 
Retorna um novo Estado inicializado para o nível especificado.
-}
iniciarJogoNoNivel :: Estado -> Int -> Estado
iniciarJogoNoNivel e nivelEscolhido =
  estadoInicial (imagens e) (e {modo = EmJogo, acaoJog = Nothing, jogoAtual = jogoEscolhido, jogoClean = jogoEscolhido, nivel = nivelEscolhido})
  where
    jogoEscolhido = case nivelEscolhido of
      2 -> jogo2
      3 -> jogo3 
      4 -> jogo4 
      _ -> jogoInicial

{-| Retorna um novo estado inicializado de acordo com o nível especificado no estado original
-}
estadoInicial :: Imagens -> Estado -> Estado
estadoInicial imgs e = case nivel e of
    1 -> Estado {modo = EmJogo,jogoAtual=jogoInicial, imagens = imgs, jogoClean = jogoInicial, acaoJog = Nothing, menu = MenuPrincipal, opcao = Jogar, nivel =1}
    2 -> Estado {modo = EmJogo,jogoAtual=jogo2, imagens = imgs, jogoClean = jogo2, acaoJog = Nothing, menu = MenuPrincipal, opcao = Jogar, nivel = 2}
    3 -> Estado {modo = EmJogo,jogoAtual=jogo3, imagens = imgs, jogoClean = jogo3, acaoJog = Nothing, menu = MenuPrincipal, opcao = Jogar, nivel = 3}
    4 -> Estado {modo = EmJogo,jogoAtual=jogo4, imagens = imgs, jogoClean = jogo4, acaoJog = Nothing, menu = MenuPrincipal, opcao = Jogar, nivel = 4}
    5 -> e {modo = Vitoria, imagens = imgs,opcao = Jogar, nivel = 4} -- muda o modo para 'Vitoria' e ajusta a opção e o nível, mantendo as imagens do estado original.
                  
{-| Retorna um novo estado inicializado para o menu principal.
Não recebe um estado existente como argumento, apenas um conjunto de imagens (Imagens).
-}
estadoInicialMenu :: Imagens -> Estado
estadoInicialMenu imgs = Estado jogoInicial imgs MenuInicial jogoInicial Nothing MenuPrincipal Jogar 1

-- Mapas implementado:

mapa01 :: Mapa
mapa01 =
  Mapa
    ((0.5, 0.5), Este)
    (5, 1.5)
    [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Plataforma, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]


mapa02 :: Mapa
mapa02 =
  Mapa
    ((0.5, 0.5), Este)
    (6.5, 10.5)
    [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Plataforma],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Plataforma, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

mapa03 :: Mapa
mapa03 =
  Mapa
    ((0.5, 0.5), Este)
    (0.5, 10.5)
    [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Plataforma, Plataforma, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Plataforma, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Plataforma],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Escada],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Escada],
      [Vazio, Plataforma, Plataforma, Plataforma, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

mapa04 :: Mapa
mapa04 =
  Mapa
    ((0.5, 0.5), Este)
    (6.5, 10.5)
    [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Plataforma, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Plataforma, Plataforma],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Plataforma, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Vazio, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

--Jogos implementados:

jogoInicial :: Jogo
jogoInicial = Jogo { mapa = mapa01,
                     inimigos = [en1,en2],
                     colecionaveis = [c1,c2],
                     jogador = jog
                   }
                   


jogo2 :: Jogo
jogo2 = Jogo { mapa = mapa02,
                     inimigos = [en3,en4],
                     colecionaveis = [c3,c4],
                     jogador = jog
                   }

jogo3 :: Jogo
jogo3 = Jogo { mapa = mapa03,
                     inimigos = [en5,en6],
                     colecionaveis = [c5,c6],
                     jogador = jog
                   }

jogo4 :: Jogo
jogo4 = Jogo { mapa = mapa04,
                     inimigos = [en7,en8],
                     colecionaveis = [c7,c8],
                     jogador = jog
                   }

acoes :: [Personagem] -> [Maybe Acao]
acoes [] = []
acoes (i:is) = (case direcao i of Norte -> Just Subir
                                  Sul -> Just Descer
                                  Este -> Just AndarDireita
                                  Oeste -> Just AndarEsquerda) : acoes is

-- Jogador implementado :
jog :: Personagem
jog = Personagem
    { velocidade = (0.0, 0.0),
      tipo = Jogador,
      posicao = (8.5, 6.5),
      direcao = Sul,
      tamanho = (0.8, 0.8),
      emEscada = False,
      ressalta = False,
      vida = 10,
      pontos = 0,
      aplicaDano = (False, 0)
    }

-- Colecionáveis e e«inimigos implementados:
c1 = (Martelo, (5.5,1))
c2 = (Moeda, (5.5,10))
c3 = (Martelo, (5.5,4))
c4 = (Moeda, (6.5,7))
c5 = (Martelo, (9.5,4))
c6 = (Moeda, (5.5,1))
c7 = (Martelo, (7,4))
c8 = (Moeda, (2.5,7))

en1 = Personagem (0.0,0.0) Fantasma (4.5,10.5) Este (1,1) False True 1 0 (False, 0.0)
en2 = Personagem (0.0,0.0) MacacoMalvado (4.5,1.5) Este (1,1) False True 10 0 (False, 0.0)
en3 = Personagem (0.0,0.0) Fantasma (2.5,7.5) Este (1,1) False True 10 0 (False, 0.0)
en4 = Personagem (0.0,0.0) MacacoMalvado (2.5,4.5) Este (1,1) False True 10 0 (False, 0.0)
en5 = Personagem (0.0,0.0) Fantasma (3.5,1.5) Este (1,1) False True 1 0 (False, 0.0)
en6 = Personagem (0.0,0.0) MacacoMalvado (6.5,7.5) Este (1,1) False True 10 0 (False, 0.0)
en7 = Personagem (0.0,0.0) Fantasma (6.5,10.5) Este (1,1) False True 1 0 (False, 0.0)
en8 = Personagem (0.0,0.0) MacacoMalvado (3.5,7.5) Este (1,1) False True 10 0 (False, 0.0)

