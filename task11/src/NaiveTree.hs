{-|
  Реализация класса типов 'Map' в виде дерева поиска,
  необязательно сбалансированного, работает за линейное
  время в худшем случае.
-}
module NaiveTree where
import Map

{-|
  Двоичное дерево поиска, необязательно сбалансированное.

  Инвариант: для любой вершины @v@:

  1. Все ключи в левом поддереве строго меньше ключа @v@.
  2. Все ключи в правом поддереве строго больше ключа @v@.
-}
data NaiveTree k a =
    -- |Пустое дерево
    Nil
    -- |@Node k a l r@ – дерево с корнем в вершине с ключом @k@,
    -- значением @a@, левым ребёнком @l@ и правым ребёнком @r@.
  | Node k a (NaiveTree k a) (NaiveTree k a)
  deriving (Show, Eq)

{-|
  @merge l r@ объединяет два дерева в одно при условии,
  что все ключи из @l@ строго меньше ключей из @r@.
-}
merge :: NaiveTree k a -> NaiveTree k a -> NaiveTree k a
merge Nil tb = tb
merge (Node k a le ri) tb = Node k a le (merge ri tb)

{-|
  Реализация функций 'Map' для 'NaiveTree'.

  'empty', 'singleton' и 'Map.null' работают за /O(1)/.
  Если /n/ – количество вершин дерева, а /h/ – высота дерева,
  то 'fromList' работает за /O(nh)/, 'toAscList' работает за /O(n^2)/,
  а 'size' работает за /O(n)/.
  Остальные функции работают за /O(h)/,
  причём каждая функция должна спускаться вниз по дереву и
  подниматься обратно не больше одного раза.

  Скорее всего, при реализации вам потребуется функция 'merge'.
-}
instance Map NaiveTree where
    empty = Nil

    singleton k a = Node k a Nil Nil

    toAscList Nil = []
    toAscList (Node k a le ri) = (toAscList le) ++ [(k, a)] ++ (toAscList ri)

    alter f k Nil = maybe Nil (singleton k) (f Nothing)
    alter f k (Node kn a le ri) | k < kn    = Node kn a (alter f k le) ri
                                | k > kn    = Node kn a le (alter f k ri)
                                | otherwise = merge (merge le res) ri 
                                  where res = maybe Nil (singleton kn) (f (Just a))

    lookup _ Nil = Nothing
    lookup k (Node kn a le ri) | kn < k    = Map.lookup k ri
                               | kn > k    = Map.lookup k le
                               | otherwise = Just a

    null Nil = True
    null _ = False

    size Nil = 0
    size (Node _ _ le ri) = 1 + (size le) + (size ri)
