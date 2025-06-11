-- (2) insere_no_fim: insere um elemento no final da lista
insere_no_fim :: t -> [t] -> [t]
insere_no_fim x [] = [x]
insere_no_fim x (c:r)
    | otherwise = c : insere_no_fim x r
