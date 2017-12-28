{-# LANGUAGE ScopedTypeVariables #-}
module Jabara.Util.MonthSpec (spec) where

import           Data.Time.Calendar
import           Jabara.Util.Month
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Text.Read

spec :: Spec
spec = do
    describe "read month" $ do
        it "readMaybe \"\"" $ do
            let r::Maybe Month = readMaybe ""
            r `shouldBe` Nothing
        prop "read test by QuickCheck" test_read


test_read :: String -> Bool
test_read s = case readMaybe s :: Maybe Month of
                Nothing -> True
                Just  m -> case toGregorian $ mDay m of
                             (_, mon, _) -> 1 <= mon && mon <= 12
