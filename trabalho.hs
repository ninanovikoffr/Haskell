-- |Trabalho de Programação Funcional 2025/1 
-- |Professor : Bruno de Oliveira Schneider

-- |Aluno: Nina Tobias Novikoff da Cunha Ribeiro
-- |Matricula : 202410375

-- |Grupo 2 : 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38

-- (2) insere_no_fim: insere um elemento no final da lista
insere_no_fim :: t -> [t] -> [t]
insere_no_fim x [] = [x]
insere_no_fim x (c:r)
    | otherwise = c : insere_no_fim x r

-- (5) concatena: junta os elementos da primeira lista com os da segunda
concatena :: [t] -> [t] -> [t]
concatena l1 l2
    | lista_vazia l1 = l2
    | otherwise     = primeiro : concatena resto l2
  where
    primeiro : resto = l1

-- verifica se a lista está vazia
lista_vazia :: [t] -> Bool
lista_vazia l
    | eh_vazia l = True
    | otherwise  = False
  where
    eh_vazia [] = True
    eh_vazia _  = False

-- (8) remover_repetidos: remove elementos repetidos mantendo a ordem da primeira ocorrência
remover_repetidos :: (Eq t) => [t] -> [t]
remover_repetidos l = aux l []
  where
    aux [] _ = []
    aux (c:r) vistos
        | pertence c vistos = aux r vistos
        | otherwise         = c : aux r (c : vistos)

-- verifica se um elemento pertence a uma lista
pertence :: (Eq t) => t -> [t] -> Bool
pertence _ [] = False
pertence e (c:r)
    | e == c    = True
    | otherwise = pertence e r

-- (11) variacoes: calcula a diferença entre elementos consecutivos da lista
variacoes :: (Integral t) => [t] -> [t]
variacoes l
    | tem_menos_de_dois l = []
    | otherwise           = (b - a) : variacoes (b:r)
  where
    a : b : r = l

-- verifica se a lista tem menos de dois elementos
tem_menos_de_dois :: [t] -> Bool
tem_menos_de_dois l
    | dois_ou_mais l = False
    | otherwise      = True
  where
    dois_ou_mais (_:_:_) = True
    dois_ou_mais _       = False

-- (14) sequencia: gera uma lista com n números começando de m
sequencia :: (Integral t) => t -> t -> [t]
sequencia n m
    | n_igual_a_zero n = []
    | otherwise        = m : sequencia (n - 1) (m + 1)
  where
    n_igual_a_zero x
        | x == 0    = True
        | otherwise = False

-- (17) uniao: junta duas listas sem repetir elementos
uniao :: (Eq t) => [t] -> [t] -> [t]
uniao l1 l2 = juntar l2 limpa1
  where
    limpa1 = remover_repetidos l1
    juntar [] res = res
    juntar (c:r) res
        | pertence c res = juntar r res
        | otherwise      = juntar r (insere_no_fim c res)

-- (20) insere_ordenado: insere um elemento em uma lista ordenada
insere_ordenado :: (Ord t) => t -> [t] -> [t]
insere_ordenado x l
    | lista_vazia l = [x]
    | x <= primeiro = x : l
    | otherwise     = primeiro : insere_ordenado x resto_lista
  where
    primeiro : resto_lista = l

-- (23) mediana: calcula a mediana de uma lista de números
mediana :: (Real t, Fractional f) => [t] -> f
mediana l
    | lista_vazia l = 0
    | eh_par tam    = media_2 (pegar_pos (meio - 1) l_ordenada, pegar_pos meio l_ordenada)
    | otherwise     = realToFrac (pegar_pos meio l_ordenada)
  where
    l_ordenada = ordenar l
    tam = tamanho l_ordenada
    meio = tam `div` 2

-- ordena usando a função insere_ordenado 
ordenar :: (Ord t) => [t] -> [t]
ordenar [] = []
ordenar (c:r) = insere_ordenado c (ordenar r)

-- verifica se número é par
eh_par :: Int -> Bool
eh_par n
    | n `mod` 2 == 0 = True
    | otherwise      = False

-- média entre dois valores
media_2 :: (Real t, Fractional f) => (t, t) -> f
media_2 (a, b) = (realToFrac a + realToFrac b) / 2

-- calcula o tamanho da lista
tamanho :: [t] -> Int
tamanho l
    | lista_vazia l = 0
    | otherwise     = 1 + tamanho (resto_lista l)

-- pega o elemento da posição i
pegar_pos :: Int -> [t] -> t
pegar_pos i (c:r)
    | i == 0    = c
    | otherwise = pegar_pos (i - 1) r

-- (26) rodar_direita: rotaciona a lista n vezes para a direita
rodar_direita :: Int -> [t] -> [t]
rodar_direita n l
    | lista_vazia l = []
    | eh_zero n     = l
    | otherwise     = rodar_direita (n - 1) (ultimo l : sem_ultimo l)

-- verifica se o número é zero
eh_zero :: Int -> Bool
eh_zero x
    | x == 0    = True
    | otherwise = False

-- retorna o último elemento da lista
ultimo :: [t] -> t
ultimo l
    | tem_um_elemento l = cabeca l
    | otherwise         = ultimo (resto_lista l)

-- remove o último elemento da lista
sem_ultimo :: [t] -> [t]
sem_ultimo l
    | tem_um_elemento l = []
    | otherwise         = cabeca l : sem_ultimo (resto_lista l)

-- verifica se a lista tem exatamente um elemento
tem_um_elemento :: [t] -> Bool
tem_um_elemento l
    | eh_uma l   = True
    | otherwise  = False
  where
    eh_uma [_] = True
    eh_uma _   = False

-- (29) media: calcula a média aritmética de uma lista de números
media :: (Real t, Fractional f) => [t] -> f
media l
    | lista_vazia l = 0
    | otherwise     = realToFrac (soma l) / realToFrac (tamanho l)

-- soma todos os elementos da lista
soma :: (Num t) => [t] -> t
soma l
    | lista_vazia l = 0
    | otherwise     = cabeca l + soma (resto_lista l)

-- retorna o primeiro elemento da lista
cabeca :: [t] -> t
cabeca (c:_) = c

-- retorna o restante da lista (sem o primeiro)
resto_lista :: [t] -> [t]
resto_lista (_:r) = r

-- (32) seleciona: retorna os elementos da lista original nas posições indicadas 
seleciona :: [Char] -> [Int] -> [[Char]]
seleciona l posicoes
    | lista_vazia posicoes = []
    | otherwise            = imprime (pegar_pos (cabeca posicoes) l : resto)
  where
    resto = map (\i -> pegar_pos i l) (resto_lista posicoes)

-- imprime: transforma lista de Char em lista de Strings
imprime :: [Char] -> [[Char]]
imprime l = map (\x -> [x]) l

-- (35) primo: verifica se um número é primo
primo :: Int -> Bool
primo n = dois_divisores n 1 0

-- verifica se o número tem exatamente dois divisores
dois_divisores :: Int -> Int -> Int -> Bool
dois_divisores n atual conta
    | atual > n          = conta == 2
    | n `mod` atual == 0 = dois_divisores n (atual + 1) (conta + 1)
    | otherwise          = dois_divisores n (atual + 1) conta

-- (38) compactar: agrupa blocos consecutivos de números repetidos
compactar :: [Int] -> [[Int]]
compactar [] = []
compactar (c:r) = agrupa_bloco c 1 r

-- agrupa números iguais que aparecem em sequência (blocos)
agrupa_bloco :: Int -> Int -> [Int] -> [[Int]]
agrupa_bloco x q [] = [finaliza x q]
agrupa_bloco x q (c:r)
    | c == x    = agrupa_bloco x (q + 1) r
    | otherwise = finaliza x q : agrupa_bloco c 1 r

-- finaliza o bloco com o número e a quantidade
finaliza :: Int -> Int -> [Int]
finaliza x q
    | q == 1    = [x]
    | otherwise = [q, x]


