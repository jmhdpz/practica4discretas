{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
-- ^ La extension de VSCode para Haskell puso automaticamente estas para ignorar algunas recomendaciones que se daban
data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

-------------------- EJERCICIO 1 --------------------
longitud :: Arbol a -> Int
longitud ArbolVacio = 0
longitud (Raiz x izq der) = 1 + longitud izq + longitud der

-------------------- EJERCICIO 2 --------------------
profundidad :: Arbol a -> Int
profundidad ArbolVacio = 0
profundidad (Raiz x izq der) = 1 + max (profundidad izq) (profundidad der)

-------------------- EJERCICIO 3 --------------------
ancho :: Arbol a -> Int
ancho ArbolVacio = 0
ancho (Raiz x ArbolVacio ArbolVacio) = 1
ancho (Raiz x izq der) =  ancho izq + ancho der

-------------------- EJERCICIO 4 --------------------
data Recorrido = InOrder | PreOrder | PosOrder

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio x = []
recorrido (Raiz x izq der) InOrder = recorrido izq InOrder ++ [x] ++ recorrido der InOrder 
recorrido (Raiz x izq der) PreOrder = [x] ++ recorrido izq PreOrder ++ recorrido der PreOrder
recorrido (Raiz x izq der) PosOrder = recorrido izq PosOrder ++ recorrido der PosOrder ++ [x]

-------------------- EJERCICIO 5 --------------------
niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz x ArbolVacio ArbolVacio) = [[x]]
niveles (Raiz x izq ArbolVacio) = [x] : niveles izq
niveles (Raiz x ArbolVacio der) = [x] : niveles der
niveles (Raiz x izq der) = [x] : concatenar (niveles izq) (niveles der)

concatenar :: [[a]] -> [[a]] -> [[a]]
concatenar [] xs = xs
concatenar xs [] = xs
concatenar (x:xs) (y:ys) = (x++y) : concatenar xs ys

-------------------- EJERCICIO 6 --------------------
minimo :: Ord a => Arbol a -> a
minimo (Raiz x ArbolVacio ArbolVacio) = x
minimo (Raiz x izq ArbolVacio) = min x (minimo izq)
minimo (Raiz x ArbolVacio der) = min x (minimo der)
minimo (Raiz x izq der) = min x (min (minimo izq) (minimo der))

-------------------- EJERCICIO 7 --------------------
maximo :: Ord a => Arbol a -> a
maximo (Raiz x ArbolVacio ArbolVacio) = x
maximo (Raiz x izq ArbolVacio) = max x (maximo izq)
maximo (Raiz x ArbolVacio der) = max x (maximo der)
maximo (Raiz x izq der) = max x (max (maximo izq) (maximo der))

-------------------- EJERCICIO 8 --------------------
eliminar :: Ord a => Arbol a -> a -> Arbol a
eliminar ArbolVacio x = ArbolVacio
eliminar (Raiz x ArbolVacio ArbolVacio) y = if x==y
                                                then ArbolVacio
                                            else Raiz x ArbolVacio ArbolVacio
eliminar (Raiz x izq ArbolVacio) y = if x == y
                                     then izq
                                     else if x < y
                                        then Raiz x (eliminar izq y) ArbolVacio
                                        else Raiz x izq ArbolVacio

eliminar (Raiz x ArbolVacio der) y = if x == y
                                     then der
                                     else if x < y
                                        then Raiz x ArbolVacio (eliminar der y)
                                        else Raiz x ArbolVacio der

eliminar (Raiz x izq der) y = if x > y
                              then Raiz x (eliminar izq y) der
                              else if y > x
                                then Raiz x izq (eliminar der y)
                                else let minDer = minimo der
                                     in Raiz minDer izq (eliminar der minDer)