module Main where
import System.IO
import Data.Char

type Doc = String
type Linha = String
type Palavra = String

main = do
    putStr "Insira o nome do arquivo: "
    hFlush stdout
    nome <- getLine
    arquivo <- readFile nome
    imprimir (construirIndice arquivo)
    -- print $ construirIndice arquivo

-- Função que executa todas as demais na devida ordem
construirIndice :: Doc -> [([Int],Palavra)]
construirIndice xs = eliminarRep(agrupar(ordenar(refiltrar(filtrar(numeraPalavras(numLinhas(lines(map toLower xs))))))))

-- Função que numera as linhas do texto, gerando uma lista de duplas
numLinhas :: [Linha] -> [(Int, Linha)]
numLinhas xs = zipar [1..] xs

-- Implementação da função zip do Prelude, que cria uma lista de duplas apartir de duas listas
zipar   [] []   = []
zipar [] (y:ys) = []
zipar (x:xs) [] = []
zipar (x:xs) (y:ys) = (x,y) : zipar xs ys

-- Divide as linhas em palavras
numeraPalavras :: [(Int, Linha)] -> [(Int, Palavra)]
numeraPalavras [] = []
numeraPalavras ((x,y):xys) = zipar (repeat x) (words y) ++ numeraPalavras xys

-- Remove as pontuações indesejadas e as numerações do texto
filtrar [] = []
filtrar ((x,y):xys) = (x, filter (\a -> isAlpha a || isSpace a) y) : filtrar xys

-- Remove as palavras com duas letras ou menos do texto
refiltrar [] = []
refiltrar ((x,y):xys) = if tamanho y > 2 then (x,y) : refiltrar xys else refiltrar xys

-- Função auxiliar que conta o tamanho de uma lista qualquer
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs 

-- Função que ordena uma lista de duplas de acordo com o primeiro elemento
ordenar :: [(Int, Palavra)] -> [(Int, Palavra)]
ordenar [] = []
ordenar (x:xs) = menor x xs : ordenar (deletar (menor x xs) (x:xs))

-- Acha a menor dupla de uma lista com base no segundo elemento
menor (a,b) [] = (a,b)
menor (a,b) ((x,y):xys) = if y < b then menor (x,y) xys else menor (a,b) xys

-- Função auxiliar que deleta a primeira aparição de um certo termo na lista
deletar _ [] = []
deletar e (x:xs) = if x == e then xs else x : deletar e xs

-- Agrupa todas as palavras repetidas em somente uma dupla
agrupar :: [(Int, Palavra)] -> [([Int], Palavra)]
agrupar [] = []
agrupar ((x,y):xys) = (listar y ((x,y):xys),y) : agrupar (remover y xys)

-- Função auxiliar que junta todas as linhas onde ocorrem uma mesma palavra em uma lista
listar _ [] = []
listar a ((x,y):xys) = if a == y then x : listar a xys else listar a xys

-- Função auxiliar que remove todas as duplas com o segundo elemento informado
remover _ [] = []
remover a ((x,y):xys) = if a == y then remover a xys else (x,y) : remover a xys

-- Elimina todas as repetições no primeiro elemento de todas as duplas numa lista de duplas
eliminarRep :: [([Int], Palavra)] -> [([Int], Palavra)]
eliminarRep [] = []
eliminarRep ((x,y):xys) = (eliminar x,y) : eliminarRep xys

-- Função auxiliar à função eliminarRep
eliminar [] = []
eliminar (x:xs) = x : eliminar (filter (/= x) xs)

-- Formata a impressão do resultado
imprimir [] = putStrLn ""
imprimir ((x,y):xys) = do
    putStr (show x)
    putStr ".\t"
    putStrLn y
    imprimir xys