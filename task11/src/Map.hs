{-|
  Определение класса типов 'Map'.
-}
module Map where

import Data.Maybe

{-|
  Поведение всех определённых здесь функций должно быть аналогично
  поведению функций из модуля "Data.Map.Strict".

  Каждую функцию, у которой здесь предложена реализация по умолчанию
  в виде 'undefined', вам требуется выразить через другую функцию из
  класса 'Map', указанную в комментарии. Например, 'fromList'
  требуется выразить через 'insert' (и, возможно, какие-то другие
  стандартные функции).

  Оставшиеся шесть функций считаются минимальной реализацией.

  Обратите внимание, что имена функций @lookup@ и @null@ совпадают
  с определёнными в стандартной библиотеке, поэтому для обращения к ним
  требуется писать @Map.lookup@ и @Map.null@, иначе компилятор не поймёт,
  какую из двух функций вы хотите.

  Строго говоря, 'alter' и 'Map.lookup' можно обобщить до функции
  вроде 'Data.Map.Strict.alterF', которая позволяет при изменении
  'Map' ещё и вытащить наружу старое значение, но мы этим заниматься
  не будем.
-}
class Map t where
    empty :: Ord k => t k a

    singleton :: k -> a -> t k a

    fromList :: Ord k => [(k, a)] -> t k a
    fromList = foldl (flip (uncurry insert)) empty

    toAscList :: t k a -> [(k, a)]

    insert :: Ord k => k -> a -> t k a -> t k a
    insert = insertWith const

    insertWith :: Ord k => (a -> a -> a) -> k -> a -> t k a -> t k a
    insertWith f k a t = alter (Just . (maybe a (f a))) k t

    insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> t k a -> t k a
    insertWithKey f k a t = insertWith (f k) k a t

    delete :: Ord k => k -> t k a -> t k a
    delete k t = alter (\_ -> Nothing) k t

    adjust :: Ord k => (a -> a) -> k -> t k a -> t k a
    adjust f k t = alter ((maybe Nothing (Just . f))) k t

    adjustWithKey :: Ord k => (k -> a -> a) -> k -> t k a -> t k a
    adjustWithKey f k t = adjust (f k) k t

    update :: Ord k => (a -> Maybe a) -> k -> t k a -> t k a
    update f k t = alter (maybe Nothing f) k t

    updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> t k a -> t k a
    updateWithKey f k t = update (f k) k t

    alter :: Ord k => (Maybe a -> Maybe a) -> k -> t k a -> t k a

    lookup :: Ord k => k -> t k a -> Maybe a

    member :: Ord k => k -> t k a -> Bool
    member k t = isJust (Map.lookup k t)

    notMember :: Ord k => k -> t k a -> Bool
    notMember k t = isNothing (Map.lookup k t)

    null :: t k a -> Bool
    null t = (size t) == 0

    size :: t k a -> Int

    check' :: Eq a => Ord k => t k a -> [(k, a)] -> Bool
    check' _ [] = True
    check' t ((k, a):xs) | ((Map.lookup k t) == Just a) = check' t xs
                         | otherwise                    = False

    check :: Eq a => Ord k => t k a -> [(k, a)] -> Bool
    check t x = ((size t) == (length x)) && (check' t x)