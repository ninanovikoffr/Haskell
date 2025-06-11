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


