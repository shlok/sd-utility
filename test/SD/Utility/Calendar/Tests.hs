--------------------------------------------------------------------------------

module SD.Utility.Calendar.Tests (tests) where

--------------------------------------------------------------------------------

import SD.Utility.Calendar

import Data.Maybe (fromJust)
import Data.Time.Calendar (Day, fromGregorian, fromGregorianValid)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck (Arbitrary, Gen, arbitrary, testProperty)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testSundayBetween_1,
      testSundayBetween_2,
      testSundayBetween_3 ]

--------------------------------------------------------------------------------

testSundayBetween_1 :: TestTree
testSundayBetween_1 =
    testCase "sundayBetween (1)" $ do
        let d1 = fromJust $ fromGregorianValid 2016 12 30
        let d2 = fromJust $ fromGregorianValid 2016 12 31
        assertBool "Expected no sunday between (1)" $
            not $ sundayBetween d1 d2
        -----
        let d1' = fromJust $ fromGregorianValid 2017 1 2
        let d2' = fromJust $ fromGregorianValid 2017 1 3
        assertBool "Expected no sunday between (2)" $
            not $ sundayBetween d1' d2'
        -----
        let d1'' = fromJust $ fromGregorianValid 2016 12 31
        let d2'' = fromJust $ fromGregorianValid 2017 1 2
        assertBool "Expected sunday between" $
            sundayBetween d1'' d2''


testSundayBetween_2 :: TestTree
testSundayBetween_2 =
    testProperty "sundayBetween (2)" $ do
        \day1 day2 -> let d1 = toDay day1
                          d2 = toDay day2
                       in sundayBetween d1 d2 == sundayBetween d2 d1


testSundayBetween_3 :: TestTree
testSundayBetween_3 =
    testProperty "sundayBetween (3)" $ do
        \theDay -> let d = toDay theDay
                    in sundayBetween d d == False


newtype MyDay = MyDay { toDay :: Day } deriving (Show)

dayGen :: Gen MyDay
dayGen = do
    y <- arbitrary
    [m,d] <- sequence $ replicate 2 arbitrary
    return . MyDay $ fromGregorian y m d

instance Arbitrary MyDay where
    arbitrary = dayGen

--------------------------------------------------------------------------------
