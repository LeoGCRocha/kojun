{---
Codigo desenvolvido por Leonardo e Andre.
Matriculas: 19102922 e XXXXXXXXX.
-> Regras:
1)Insira um número em cada célula do diagrama de forma que cada região de tamanho N contenha cada número de 1 a N exatamente uma vez.
2)Os números nas células ortogonalmente adjacentes devem ser diferentes.
3) Se duas células estiverem verticalmente adjacentes na mesma região, o número da célula superior deve ser maior do que o número da célula inferior.
--}
-- tabuleiro 6x6(base)
-- sistema de coordenadas, para acessar uma matriz do tipo parametros: (linha, coluna)
type Coordenada = (Int, Int)

-- sistemas de pontos, com uma tupla onde primeiro ponto é o valor e o segundo ponto é seu grupo. parametros: (valor numerico, grupo)
-- campos vazios seram representados com 0
type Ponto = (Int, Int)

mat = [[(0, 0), (0, 1), (4, 1), (0, 1), (2, 3), (0, 4)], [(0, 0), (0, 1), (3, 2), (0, 3), (0, 3), (0, 3)], [(1, 0), (4, 0), (0, 5), (4, 3), (0, 10), (0, 10)], [(0, 6), (5, 7), (0, 5), (0, 9), (0, 9), (2, 10)], [(0, 6), (0, 7), (0, 7), (0, 8), (3, 8), (0, 10)], [(6, 7), (2, 7), (0, 7), (2, 8), (0, 8), (5, 8)]]

-- Metodos para os pontos
-- Retonar o valor de um ponto
pegarValorPonto :: Ponto -> Int
pegarValorPonto (_, x) = x

-- Retornar o grupo de um ponto
pegarGrupoPonto :: Ponto -> Int
pegarGrupoPonto (x,_) = x

-- retorna um elemento na coordenada especificada, o segundo parametro de ponto define o tamanho do tabuleiro, ou seja o limite do jogo.
-- (-1,-1) é o retorno padrão para erros
pegarPontoCoordernada :: Coordenada -> [[Ponto]]-> Ponto 
pegarPontoCoordernada (x,y) mat | ((x < 0) || (x > length mat)) = (-1, -1) -- array index out of range em x
  | ((y < 0) || (y > length mat)) = (-1,-1) -- array index out of range em y
  | otherwise = mat !! x !! y -- mat !! 0 !! 0 percorre o indice 0 , 0 por exemplo.

main :: IO ()
main = do
  -- Demonstração metodos, para testes
  print(pegarValorPonto (1,2))
  print(pegarGrupoPonto (1,2))
  print(pegarPontoCoordernada (1,1) mat) -- teste para pegar um ponto