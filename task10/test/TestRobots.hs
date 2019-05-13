import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 40 50
        peter  = robot "Peter" 30 100
        steven  = robot "Steven" 20 100
        dead   = robot "dead" 10000 0
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"

        , testCase "Test for getAttack" $
            getAttack walter @?= 40

        , testCase "Test for getHealth" $
            getHealth walter @?= 50

        , testCase "Test for setName" $
            setName "Tom" walter @?= robot "Tom" 40 50

        , testCase "Test for setAttack" $
            setAttack 100 walter @?= robot "Walter" 100 50

        , testCase "Test for setHealth" $
            setHealth 100 walter @?= robot "Walter" 40 100

        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 40, health: 50"

        , testCase "Test for damage" $
            damage walter 30 @?= robot "Walter" 40 20

        , testCase "Test for isAlive(dead)" $
            isAlive dead @?= False

        , testCase "Test for isAlive(alive)" $
            isAlive walter @?= True

        , testCase "Test for fight" $
            fight walter peter @?= robot "Peter" 30 60

        , testCase "Test for getWinner" $
            getWinner (steven, peter) @?= steven

        , testCase "Test for fightNRounds even" $
            fightNRounds 10 steven peter @?= (setHealth (-50) steven, setHealth 20 peter)

        , testCase "Test for fightNRounds odd" $
            fightNRounds 5 steven peter @?= (setHealth 40 steven, setHealth 40 peter)

        , testCase "Test for threeRoundFight" $
            threeRoundFight walter peter @?= robot "Walter" 40 20

        , testCase "Test for survivors" $
            survivors @?= [robot "a" 100 600]
        ]
