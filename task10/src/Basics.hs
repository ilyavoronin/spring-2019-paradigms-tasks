module Basics where
import Prelude hiding (head, tail, take, drop, filter, foldl, concat, (++))

-- Цель первой части домашнего задания -- познакомить вас с основами синтаксиса Хаскеля
-- В этом задании запрещается использовать какие-либо функции из стандартной библиотеки,
-- кроме конструкторов списков и операторов сравнения из `Ord`.
-- Также запрещается использовать list comprehension.
-- Однако разрешается использовать функции, реализованные самостоятельно.

-- 1. head' возвращает первый элемент непустого списка
head' :: [a] -> a
head' (x:list) = x

-- 2. tail' возвращает список без первого элемента, для пустого - пустой
tail' :: [a] -> [a]
tail' [] = []
tail' (list:x) = x

-- 3. take' возвращает первые n >= 0 элементов исходного списка
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n list = (head' list):(take' (n - 1) (tail' list))

-- 4. drop' возвращает список без первых n >= 0 элементов; если n больше длины
-- списка, то пустой список.
drop' :: Int -> [a] -> [a]
drop' 0 list = list
drop' n list = drop' (n - 1) (tail' list)

-- 5. filter' возвращает список из элементов, для которых f возвращает True
filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f list | (f (head' list)) = (head' list):(filter' f (tail' list))
               | otherwise     = filter' f (tail' list)

-- 6. foldl'' последовательно применяет функцию f к элементу списка l и значению,
-- полученному на предыдущем шаге, начальное значение
-- foldl'' (+) 0 [1, 2, 3] == (((0 + 1) + 2) + 3)
-- foldl'' (*) 4 [] == 4
foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' f z [] = z
foldl'' f z l = foldl'' f (f z (head' l)) (tail' l)

-- 7. concat' принимает на вход два списка и возвращает их конкатенацию
-- concat' [1,2] [3] == [1,2,3]
concat' :: [a] -> [a] -> [a]
concat' [] list2 = list2
concat' list1 list2 = (head' list1):(concat' (tail' list1) list2)

-- 8. quickSort' возвращает его отсортированный список
-- quickSort' должен быть реализован через алгоритм QuickSort
-- (выбор pivot может быть любым)
quickSort' :: Ord a => [a] -> [a]
quickSort' = undefined
