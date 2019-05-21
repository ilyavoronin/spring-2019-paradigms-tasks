{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации "Map"
  с определённым именем.

  Мы хотим писать тесты один раз для всех возможных реализаций "Map".
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса "Map", который мы хотим протестировать.

  Специально для этих целей существует обёртка "Data.Proxy", он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testGroup "Smoke tests" [
            testCase "toAscList . fromList sorts list" $
                let tr = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                toAscList tr @?= [(1, "x"), (2, "a"), (3, "c")]
        ],

        testGroup "insert" [
            testCase "insert existing key" $
                check (insert 7 "x" (fromList [(5, "a"), (3, "b")] :: m Int String)) [(3, "b"), (5, "a"), (7, "x")] @?= True
            ,
            testCase "insert new key" $
                check (insert 7 "x" (fromList [(5, "a"), (3, "b")] :: m Int String)) [(3, "b"), (5, "a"), (7, "x")] @?= True
            ,
            testCase "insert in empty map" $
                check (insert 5 "x" (empty :: m Int String)) [(5, "x")] @?= True
        ],

        testGroup "insertWith" [
            testCase "insert existing key" $
                check (insertWith (++) 5 "xxx" (fromList [(5, "a"), (3, "b")] :: m Int String)) [(3, "b"), (5, "xxxa")] @?= True
            ,
            testCase "insert new key" $
                check (insertWith (++) 7 "xxx" (fromList [(5, "a"), (3, "b")] :: m Int String)) [(3, "b"), (5, "a"), (7, "xxx")] @?= True
            ,
            testCase "insert in empty map" $
                check (insertWith (++) 5 "xxx" (empty :: m Int String)) [(5, "xxx")] @?= True
        ],

        testGroup "insertWithKey" [
            let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value in
            testCase "insert existing key" $
                check (insertWithKey f 5 "xxx" (fromList [(5, "a"), (3, "b")] :: m Int String)) [(3, "b"), (5, "5:xxx|a")] @?= True
            ,
            let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value in
            testCase "insert new key" $
                check (insertWithKey f 7 "xxx" (fromList [(5, "a"), (3, "b")] :: m Int String)) [(3, "b"), (5, "a"), (7, "xxx")] @?= True
            ,
            let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value in
            testCase "insert in empty map" $
                check (insertWithKey f 5 "xxx" (empty :: m Int String)) [(5, "xxx")] @?= True
        ],

        testGroup "delete" [
            testCase "delete existing key" $
                check (delete 5 (fromList [(5, "a"), (3, "b")] :: m Int String)) [(3, "b")] @?= True
            ,
            testCase "delete non-existing key" $
                check (delete 7 (fromList [(5, "a"), (3, "b")] :: m Int String)) [(3, "b"), (5, "a")] @?= True
            ,
            testCase "delete from empty map" $
                check (delete 5 (empty :: m Int String)) [] @?= True
        ],

        testGroup "adjust" [
            testCase "adjust existing key" $
                check (adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")] :: m Int String)) [(3, "b"), (5, "new a")] @?= True
            ,
            testCase "adjust non-existing key" $
                check (adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")] :: m Int String)) [(3, "b"), (5, "a")] @?= True
            ,
            testCase "adjust key from empty map" $
                check (adjust ("new " ++) 7 (empty :: m Int String)) [] @?= True
        ],

        testGroup "adjustWithKey" [
            testCase "adjust existing key" $
                let f key x = (show key) ++ ":new " ++ x in
                check (adjustWithKey f 5 (fromList [(5,"a"), (3,"b")] :: m Int String)) [(3, "b"), (5, "5:new a")] @?= True
            ,
            testCase "adjust non-existing key" $
                let f key x = (show key) ++ ":new " ++ x in
                check (adjustWithKey f 7 (fromList [(5,"a"), (3,"b")] :: m Int String)) [(3, "b"), (5, "a")] @?= True
            ,
            testCase "adjust key from empty map" $
                let f key x = (show key) ++ ":new " ++ x in
                check (adjustWithKey f 7 (empty :: m Int String)) [] @?= True
        ],

        testGroup "update" [
            testCase "update existing key" $
                let f x = if x == "a" then Just "new a" else Nothing in
                check (update f 5 (fromList [(5,"a"), (3,"b")] :: m Int String)) [(3, "b"), (5, "new a")] @?= True
            ,
            testCase "update non-existing key" $
                let f x = if x == "a" then Just "new a" else Nothing in
                check (update f 7 (fromList [(5,"a"), (3,"b")] :: m Int String)) [(3, "b"), (5, "a")] @?= True
            ,
            testCase "update deletes key" $
                let f x = if x == "a" then Just "new a" else Nothing in
                check (update f 3 (fromList [(5,"a"), (3,"b")] :: m Int String)) [(5, "a")] @?= True
        ],

        testGroup "updateWithKey" [
            testCase "update existing key" $
                let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing in
                check (updateWithKey f 5 (fromList [(5,"a"), (3,"b")] :: m Int String)) [(3, "b"), (5, "5:new a")] @?= True
            ,
            testCase "update non-existing key" $
                let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing in
                check (updateWithKey f 7 (fromList [(5,"a"), (3,"b")] :: m Int String)) [(3, "b"), (5, "a")] @?= True
            ,
            testCase "update deletes key" $
                let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing in
                check (updateWithKey f 3 (fromList [(5,"a"), (3,"b")] :: m Int String)) [(5, "a")] @?= True
        ],

        testGroup "member" [
            testCase "member" $
                member 5 (fromList [(5, "a"), (3, "b")] :: m Int String) @?= True
            ,
            testCase "not member" $
                member 1 (fromList [(5, "a"), (3, "b")] :: m Int String) @?= False
        ],

        testGroup "notMember" [
            testCase "member" $
                notMember 5 (fromList [(5, "a"), (3, "b")] :: m Int String) @?= False
            ,
            testCase "not member" $
                notMember 1 (fromList [(5, "a"), (3, "b")] :: m Int String) @?= True
        ], 

        testGroup "null" [
            testCase "size == 0" $
                Map.null (empty :: m Int String) @?= True
            ,
            testCase "size != 0" $
                Map.null (fromList [(5, "a"), (3, "b")] :: m Int String) @?= False
        ]
    ]

{-
testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 1 "a" Nil (Node 2 "b" Nil Nil)
        ]
    ]
-}

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList){-,
        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
        testNaiveTree-}
    ]
