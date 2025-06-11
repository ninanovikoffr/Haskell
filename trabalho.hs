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
