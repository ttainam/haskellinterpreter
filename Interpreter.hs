module Interpreter where

import Parser 

-- Definição de tipos
---------------------
data Ty = TBool
        | TNum 
        | TPares Ty Ty
        deriving Show

-- Função que avalia um passo de execução
-- Adicionar os novos operadores (Mult e Or)
--------------------------------------------
step :: Expr -> Maybe Expr
-- S-Add
step (Add (Num n1) (Num n2)) = Just (Num (n1 + n2))
-- S-Add2 
step (Add (Num n) e2) = 
    case (step e2) of
        Just e2' -> Just (Add (Num n) e2')
        Nothing  -> Nothing                        
-- S-Add1
step (Add e1 e2) = 
    case (step e1) of 
        Just e1' -> Just (Add e1' e2)
        Nothing  -> Nothing
-- S-And2
step (And BTrue e2) = Just e2
-- S-And3
step (And BFalse e2) = Just BFalse
-- S-And1
step (And e1 e2) = 
    case (step e1) of 
        Just e1' -> Just (And e1' e2)
        Nothing  -> Nothing
step (If BTrue e1 e2) = Just e1
step (If BFalse e1 e2) = Just e2
step (If e1 e2 e3) = case (step e1) of
                       Just e1' -> Just (If e1' e2 e3)
                       Nothing  -> Nothing

-----------------------------------------------------------------------------------------------
-- Ajustado código para AND, usado como base o Add
-----------------------------------------------------------------------------------------------
-- S-Mult
step (Mult (Num n1)  (Num n2)) = Just (Num (n1 * n2))
-- S-Mult2
step (Mult (Num n) e2) = 
    case (step e2) of
        Just e2' -> Just (Mult (Num n) e2')
        Nothing  -> Nothing   
-- S-Mult1
step (Mult e1 e2) = 
    case (step e1) of 
        Just e1' -> Just (Mult e1' e2)
        Nothing  -> Nothing      

-----------------------------------------------------------------------------------------------
-- Ajustado código para OR, usado como base o And, porem inverte o sinal
-----------------------------------------------------------------------------------------------
-- S-Or1
step (Or BTrue e2) = Just BTrue
-- S-Or2
step (Or BFalse e2) = Just e2
-- S-Or
step (Or e1 e2) = 
    case (step e1) of 
        Just e1' -> Just (Or e1' e2)
        Nothing  -> Nothing

-----------------------------------------------------------------------------------------------
-- Ajustado código para Pares, usado como base o If
-----------------------------------------------------------------------------------------------
step (Pair e1 e2) = case (step e1, step e2) of
                        (Just e1', Just e2') -> Just (Pair e1' e2')
                        _                    -> Nothing
step (primeiro (Pair e1 e2)) = Just e1
step (segundo (Pair e1 e2)) = Just e2
step e = Just e

-- Função que avalia uma expressão até apresentar um
-- resultado ou gerar um erro
-- Não precisa alterar
----------------------------------------------------
eval :: Expr -> Maybe Expr
eval e = case (step e) of 
           Just e' -> if (e == e') then
                        Just e
                      else
                        eval e'
           _ -> error "Semantic error: erro avaliando expressão!" 

-- Função que verifica o tipo de uma expressão
-- Adicionar os novos operadores (Mult e Or)
----------------------------------------------
typeof :: Expr -> Maybe Ty 
typeof BTrue = Just TBool
typeof BFalse = Just TBool
typeof (Num _) = Just TNum
typeof (Add e1 e2) = case (typeof e1) of
                       Just TNum -> case (typeof e2) of
                                      Just TNum -> Just TNum
                                      _         -> Nothing -- erro de tipo
                       _         -> Nothing -- erro de tipo
typeof (And e1 e2) = case (typeof e1, typeof e2) of 
                       (Just TBool, Just TBool) -> Just TBool
                       _                        -> Nothing -- erro de tipo
typeof (If e1 e2 e3) = case (typeof e1) of 
                         Just TBool -> case (typeof e2, typeof e3) of 
                                         (Just TBool, Just TBool) -> Just TBool
                                         (Just TNum, Just TNum)   -> Just TNum
                                         _                        -> Nothing -- erro de tipo
                         _          -> Nothing -- erro de tipo
-----------------------------------------------------------------------------------------------
-- Ajustado código para Pares, usado como base o AND
-----------------------------------------------------------------------------------------------
typeof (Or e1 e2) = case (typeof e1, typeof e2) of 
                       (Just TBool, Just TBool) -> Just TBool
                       _                        -> Nothing -- erro de tipo
-----------------------------------------------------------------------------------------------
-- Ajustado código para Pares, usado como base o ADD
-----------------------------------------------------------------------------------------------
typeof (Mult e1 e2) = case (typeof e1) of
                       Just TNum -> case (typeof e2) of
                                      Just TNum -> Just TNum
                                      _         -> Nothing -- erro de tipo
                       _         -> Nothing -- erro de tipo
-----------------------------------------------------------------------------------------------
-- Ajustado código para Pares, usado como base o AND
-----------------------------------------------------------------------------------------------
typeof (Pair e1 e2) = case (typeof e1, typeof e2) of 
                       (Just num1, Just num2) -> Just (TPares num1 num2)
                       _                        -> Nothing -- erro de tipo
typeof (Primeiro e) = case (typeof e) of 
                       Just (TPares num1 num2) -> Just (TPares num1 num2)
                       _                        -> Nothing -- erro de tipo
typeof (Segundo e) = case (typeof e1, typeof e2) of 
                       Just (TPares num1 num2) -> Just (TPares num1 num2)
                       _                        -> Nothing -- erro de tipo


-- Função que faz a verificação de tipos
-- Não precisa alterar
----------------------------------------
typecheck :: Expr -> Expr
typecheck e = case (typeof e) of 
                Just _ -> e
                _ -> error "Type error: erro na verificação de tipos!"


------------------------------------------
-- Ler os códigos e chamar o interpretador
------------------------------------------
main = getContents >>= print . eval . typecheck . parser . lexer 

