{---
Codigo desenvolvido por Leonardo e Andre.
Matriculas: 19102922 e 19150871.
-> Regras:
1) Insira um número em cada célula do diagrama de forma que cada região de tamanho N contenha cada número de 1 a N exatamente uma vez.
2) Os números nas células ortogonalmente adjacentes devem ser diferentes.
3) Se duas células estiverem verticalmente adjacentes na mesma região, o número da célula superior deve ser maior do que o número da célula inferior.
--}
-- sistema de coordenadas, para acessar uma matriz do tipo parametros: (linha, coluna)
-- Esses links deram base para enteder alguns conceitos:
-- ref#1: https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/
-- ref#2http://www.cse.chalmers.se/edu/year/2010/course/TDA555/lab3.html
-- docs: https://docs.google.com/document/d/1fXurGjbriB75zIhL7agJ220ZSGUsI-GTU4fbOJ1trDY/edit?usp=sharing
type Coordenada = (Int, Int)

-- Sistemas de pontos, com uma tupla onde primeiro ponto é o valor e o segundo ponto é seu grupo. parametros: (valor numerico, grupo)
-- Campos vazios seram representados com 0
type Ponto = (Int, Int)
m = [[(0, 0), (0, 1), (4, 1), (0, 1), (2, 3), (0, 4)], [(0, 0), (0, 1), (3, 2), (0, 3), (0, 3), (0, 3)], [(1, 0), (4, 0), (0, 5), (4, 3), (0, 10), (0, 10)], [(0, 6), (5, 7), (0, 5), (0, 9), (0, 9), (2, 10)], [(0, 6), (0, 7), (0, 7), (0, 8), (3, 8), (0, 10)], [(6, 7), (2, 7), (0, 7), (2, 8), (0, 8), (5, 8)]]

-- Metodos para os pontos
-- Retonar o valor de um ponto
pegarValorPonto :: Ponto -> Int
pegarValorPonto (x, _) = x

-- Retornar o grupo de um ponto
pegarGrupoPonto :: Ponto -> Int
pegarGrupoPonto (_,y) = y

pegarX:: Coordenada -> Int
pegarX (x,_) = x

pegarY :: Coordenada -> Int
pegarY (_,y) = y

-- retorna um elemento na coordenada especificada, o segundo parametro de ponto define o tamanho do tabuleiro, ou seja o limite do jogo.
-- (-1,-1) é o retorno padrão para erros
-- https://stackoverflow.com/questions/5217171/how-can-i-get-nth-element-from-a-list
pegarPontoCoordernada :: Coordenada -> [[Ponto]]-> Ponto 
pegarPontoCoordernada (x,y) m 
  | (x < 0) || (x > length m - 1) = (-1, -1)
  | (y < 0) || (y > (length (head m) - 1)) = (-1, -1)
  | otherwise = m !! x !! y

-- Retorna todos as coordenadas que fazem parte da posição atual.
pegarCoordenadasPorGrupo :: Coordenada -> [[Ponto]] -> [Coordenada]
-- passando como parametro o ponto na coordenada especifica para obter seu grupo.
-- exemplo let ponto = pegarPontoCoordenada cord mat, não consegui fazer.
-- auxliarGrupo ponto (pegarGrupoPonto ponto) mat 
pegarCoordenadasPorGrupo cord mat = auxiliarGrupo cord (pegarGrupoPonto (pegarPontoCoordernada cord m)) m

auxiliarGrupo :: Coordenada -> Int -> [[Ponto]]->[Coordenada]
-- usando list compreeshion para construir o vetor de coordenadas, num representa o grupo especifico
-- A recebendo os valores da mat[], B recebendo os valores da mat em [][]
-- Condição é se o grupo atual for igual o grupo solicitado
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html
auxiliarGrupo (x,y) grupo m = [(a, b) | a <- [0..(length m)], b <- [0..(length (head m))], pegarGrupoPonto(pegarPontoCoordernada (a,b) m) == grupo, (x,y) /= (a,b)] 

-- Procura a primeira cordenada vazia (0,_). Caso não seja encontrada nenhuma significa que o tabuleiro já esta motando então retorna (-1,-1) 
coordenadaVazia :: [[Ponto]] -> Coordenada
coordenadaVazia m = verificaVazia (0,0) m -- É enviado a coordenada inicial para partir dela como referência.

-- Metodo de ajuste para o segundo retorno de vazio, primeiro verificar se as posições antendem os limites maximos da matriz.
-- Caso sim, chama o proximo metodo de comparação para verificar se o valor na coordenada é valido.
verificaVazia :: Coordenada -> [[Ponto]] -> Coordenada
verificaVazia cord m 
  | (pegarX cord == length m) && (pegarY cord == length (head m)) = (-1, -1) -- Verificando se indice foi estourado
  | pegarY cord == length (head m) = verificarValorVazio cord m
  | pegarValorPonto(pegarPontoCoordernada cord m) == 0 = cord -- Ponto já nulo, retorna ele.
  | otherwise  = verificaVazia (pegarX cord, pegarY cord + 1) m -- Ajustando indice em (_,y) para deslocar para proxima coordeanda.

verificarValorVazio :: Coordenada -> [[Ponto]] -> Coordenada
verificarValorVazio cord m 
  | pegarValorPonto (pegarPontoCoordernada cord m) == 0 = cord 
  | otherwise  = verificaVazia (pegarX cord + 1, 0) m -- Ajudando indice em (x,_) para deslocar para proxima coordenada.

-- Metodo para troca de elemento da lista em uma dada posicao
-- Recebe um ponto da matriz [(x,y)] x -> valor do ponto | y -> valor do grupo
-- Recebe um indicador do grupo -> z 
-- Recebe um indicador do ponto -> w
-- Recebe um indicador do novo ponto a ser recebido -> p
-- Retorno do novo ponto para reposicionamento -> [Ponto]
reposicionaponto :: [Ponto] -> Int -> Int -> Ponto -> [Ponto]
reposicionaponto [] _ _ _ = [(-1,-1)]
reposicionaponto (x:y) z w p
  | z == w = p:y
  | otherwise = x: reposicionaponto y z (w + 1) p 

-- Metodo para troca de linha da matriz de elementos em uma dada posição
-- Recebe um vetor de pontos da matriz -> [[Ponto]]
-- Recebe o valor da nova linha da matriz -> z
-- Recebe o valor da nova coluna da matriz -> w
-- Recebe o valor da nova linha da matriz -> l
reposicionalinha :: [[Ponto]] -> Int -> Int -> [Ponto] -> [[Ponto]]
reposicionalinha [] _ _ _ = [[(-1,-1)]]
reposicionalinha (x:y) z w l
    | z == w = l:y
    | otherwise = x:reposicionalinha y z (w + 1) l

-- Metdodo para troca de coordenada da matriz, levando em conta a posicao especificada
troca :: Coordenada -> Ponto -> [[Ponto]] -> [[Ponto]]
troca (x, y) ponto matriz
    | (x < 0) || (x > length matriz -1) = error "posicao fora da matriz" -- verificação se a posicao de troca de x é valida, caso seja, parte para a verificação da posição y, caso contrário a posição é invalida
    | (y < 0) || (y > (length (head matriz) -1)) = error "posicao fora da matriz" -- verificação se a posicao de y é valida, caso seja, parte para o posicionamento do ponto na coordenada desginada, caso contrário a posicao é invalida 
    | otherwise = reposicionalinha matriz x 0 (reposicionaponto (matriz!!x) y 0 ponto) -- posicionamento do ponto na coordenada desejada, por meio da chamada recursiva de reposicionaponto, reposicionalinha

-- Metodo para fazer a verificação se um numero passado como parametro, não foi alocado no grupo.
-- O metodo verify list é usado como auxiliar para este, dando suporte as operações.
-- Para o metodo verificar lista é passado todos os pontos que pertecem ao mesmo grupo da coordenada especificada na chamada.
verificarNumeroCoordGrupo :: Int -> Coordenada -> [[Ponto]] -> Bool
verificarNumeroCoordGrupo num cord matriz = verificarNaLista num (pegarCoordenadasPorGrupo cord matriz) matriz

-- Metodo para verificar se o numero passado é valido para uma coordenada (x,y)
verificarNum :: Int -> Coordenada -> [[Ponto]] -> Bool
verificarNum num cord matriz = (verificarNumeroCoordGrupo num cord matriz) && (verificaCoordAdj num cord matriz) 
-- Caso de tese para veficar se a coord adj esta valida, se não ira retornar false para o programa.
f :: Coordenada -> Bool
f (x,y) 
  | x < 0 || y < 0 = False
  | otherwise  = True
-- Metodo para verificacao se a lista contem todas as coordenadas adjacentes a cooordenada procurada
-- retorna a lista de elementos adjacentes ao elemento especificado
getCoordAdj :: Coordenada -> [Coordenada]
getCoordAdj (x,y) = filter f [(x+1,y), (x-1,y), (x,y+1), (x,y-1)] 

--- verifyneib
-- Metodo para verificacao do numero passado ser encontrado em coordenadas adjacentes
-- por meio da chamada recursiva do metodo GetCoordAdj
-- retorna um valor Booleano
verificaCoordAdj :: Int -> Coordenada -> [[Ponto]] -> Bool
verificaCoordAdj num cord matriz = verificarNaLista num (getCoordAdj cord) matriz && verificarCordAdjSupInf cord matriz

-- Metodo para verificacao da Lista de pontos se é igual ao valor procurado
verificarNaLista :: Int -> [Coordenada] -> [[Ponto]] -> Bool
verificarNaLista _ [] _   = True
verificarNaLista num (x:y) matriz
  | pegarValorPonto(pegarPontoCoordernada x matriz) == num = False
  | otherwise = verificarNaLista num y matriz

-- Vertificando se duas cedulas forem duas regiões iguais, a superior deve ser maior que a inferior
verificarCordAdjSupInf :: Coordenada -> [[Ponto]] -> Bool
verificarCordAdjSupInf (x,y) m 
    | y == 0 = True
    | pegarGrupoPonto(pegarPontoCoordernada2 (x, y-1) m) /= pegarGrupoPonto(pegarPontoCoordernada2 (x,y) m) = True
    | pegarValorPonto(pegarPontoCoordernada2 (x,y-1) m) >= pegarValorPonto(pegarPontoCoordernada2 (x,y) m) = True
    | otherwise = False

-- Metodo retorna números que podem completar uma coordenada, onde é aplicado o filtro de verificação para fazer checagens sobre 
-- a possibilidade do numero estar na posição.
numValidos :: Coordenada -> [[Ponto]] -> [Int]
numValidos cord m = [n | n <- [1..(length(pegarCoordenadasPorGrupo cord m)+1)], verificarNum n cord m]

-- Metodos finais, falta fazer logicas de verificação para encaixar com eles.
-- Terminar os dois metodos de solve.
-- Aplicar regra de verificação no grupo.
-- Recebe a coordenada que sera testada, a matriz total e os numeros possiveis para a coordenada especifica.
-- Nessee metodo uma matriz nova vai sendo reconstruida atraves de chamadas recursivas

-- resolve o puzzle utilizando backtracking
soluciona :: Coordenada -> [[Ponto]] -> [Int] -> [[Ponto]]
soluciona _ _ [] = [] -- matriz vazia de entrada retorno padrão.
soluciona (-1,-1) m _ = m -- posição invalida retorno deve ser o mesmo
soluciona (x,y) m (i:is)
  | null solucao1 = soluciona (x,y) m is -- Posição atual vazia, executar teste novamente.
  | otherwise = solucao1
  where 
    solucionando (x,y) m = soluciona (coordenadaVazia m) m (numValidos (coordenadaVazia m) m)--teste
    solucao1  = solucionando (x,y) (troca (x, y) (i, pegarGrupoPonto(pegarPontoCoordernada (x, y) m)) m )

solucionador :: [[Ponto]] -> [[Ponto]]
solucionador m = soluciona (0,0) m (numValidos (0,0) m)

main :: IO ()
main = do
  print(solucionador m) -- Resultado final com solução da matriz