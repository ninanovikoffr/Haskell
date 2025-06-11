-- (2) insere_no_fim: insere um elemento no final da lista
insere_no_fim :: t -> [t] -> [t]
insere_no_fim x [] = [x]
insere_no_fim x (c:r)
    | otherwise = c : insere_no_fim x r

-- (5) concatena: junta os elementos da primeira lista com os da segunda
concatena :: [t] -> [t] -> [t]
concatena l1 l2
    | null_lista l1 = l2
    | otherwise     = primeiro : concatena resto l2
  where
    null_lista [] = True
    null_lista _  = False
    primeiro : resto = l1

-- (8) remover_repetidos: remove elementos repetidos mantendo a ordem da primeira ocorrência
remover_repetidos :: (Eq t) => [t] -> [t]
remover_repetidos l = aux l []
  where
    aux [] _ = []
    aux (c:r) vistos
        | pertence c vistos = aux r vistos
        | otherwise         = c : aux r (c:vistos)

-- Função auxiliar para verificar se um elemento pertence a uma lista
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
    a:b:r = l

-- função auxiliar para verificar se a lista tem menos de dois elementos
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
    limpa2 = remover_repetidos l2
    juntar [] res = res
    juntar (c:r) res
        | pertence c res = juntar r res
        | otherwise      = juntar r (insere_no_fim c res)

-- (20) insere_ordenado: insere um elemento em uma lista ordenada
insere_ordenado :: (Ord t) => t -> [t] -> [t]
insere_ordenado x l
    | lista_vazia l = [x]
    | x <= primeiro = x : l
    | otherwise     = primeiro : insere_ordenado x resto
  where
    primeiro : resto = l

-- função auxiliar para verificar se a lista está vazia
lista_vazia :: [t] -> Bool
lista_vazia l
    | eh_vazia l = True
    | otherwise  = False
  where
    eh_vazia [] = True
    eh_vazia _  = False

-- (23) mediana: calcula a mediana de uma lista de números
mediana :: (Real t, Fractional f) => [t] -> f
mediana l
    | lista_vazia l = 0
    | eh_par tam    = media (pegar_posicao (meio - 1) l_ordenada, pegar_posicao meio l_ordenada)
    | otherwise     = realToFrac (pegar_posicao meio l_ordenada)
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
media :: (Real t, Fractional f) => (t, t) -> f
media (a, b) = (realToFrac a + realToFrac b) / 2

-- calcula o tamanho da lista
tamanho :: [t] -> Int
tamanho l
    | lista_vazia l = 0
    | otherwise     = 1 + tamanho (resto l)
  where
    resto (_:r) = r
    resto []    = []

-- pega o elemento da posição i
pegar_posicao :: Int -> [t] -> t
pegar_posicao i l
    | i == 0    = primeiro l
    | otherwise = pegar_posicao (i - 1) (resto l)
  where
    primeiro (c:_) = c
    primeiro []    = error "lista vazia"
    resto (_:r)    = r
    resto []       = error "índice fora da lista"
