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
type Coordenadas = (Int, Int)
-- sistemas de pontos, com uma tupla onde primeiro ponto é o valor e o segundo ponto é seu grupo. parametros: (valor numerico, grupo)
-- campos vazios seram representados com -1
type Pontos = (Int, Int)
mat = [
	[(0, 0), (0, 0), (0, 0), (0, 0),  (0, 0),  (0, 0),  (0, 0)],
	[(0, 0), (0, 0), (0, 0), (0, 0),  (0, 0),  (0, 0),  (0, 0)],
	[(0, 0), (0, 0), (0, 0), (0, 0),  (0, 0), (0, 0), (0, 0)],
	[(0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0),  (0, 0)],
	[(0, 0), (0, 0), (0, 0), (0, 0), (0, 0),  (0, 0),  (0, 0)],
	[(0, 0), (0, 0), (0, 0), (0, 0),  (0, 0),  (0, 0),  (0, 0)]
    ]


main = do
    putStrLn "Get started!"