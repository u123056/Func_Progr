{-# OPTIONS_GHC -Wall #-}

module Luhn where

-- Проверка корректности списка цифр (чисел от 0 до 9) алгоритмом Луна. 
-- Алгоритм:
-- 1. Все числа, стоящие на чётных местах (считая с конца), удваиваются. Если при этом получается число, большее 9, то из него вычитается 9. Числа, стояшие на нечётных местах, не изменяются.
-- То есть: последнее число не меняется; предпоследнее удваивается; 3-е с конца (предпредпоследнее) не меняется; 4-е с конца удваивается и т.д.
-- 2. Все полученные числа складываются.
-- 3. Если полученная сумма кратна 10, то исходный список корректен.

-- Не пытайтесь собрать всё в одну функцию, используйте вспомогательные.
-- Не забудьте добавить тесты (в отдельном модуле или здесь же), в том числе для вспомогательных функций!
isLuhnValid :: [Int] -> Bool
isLuhnValid = error "todo"
