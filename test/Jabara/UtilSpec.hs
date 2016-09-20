module Jabara.UtilSpec (spec) where

import GHC.Base
import Jabara.Util
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
    describe "overlap" $ do
        it "overlap (10::Int,20) (11,13)" $ do
            overlap (10::Int,20) (11,13) `shouldBe` Just (11,13)
        it "overlap (11::Int,13) (10,20)" $ do
            overlap (11::Int,13) (10,20) `shouldBe` Just (11,13)
        it "overlap (13::Int,11) (20,10)" $ do
            overlap (13::Int,11) (20,10) `shouldBe` Just (11,13)
        it "overlap (10::Int,20) (0,5)" $ do
            overlap (10::Int,20) (0,5) `shouldBe` Nothing
        it "overlap (10::Int,20) (0,10)" $ do
            overlap (10::Int,20) (0,10) `shouldBe` Just (10,10)
        it "overlap (10::Int,20) (10,15)" $ do
            overlap (10::Int,20) (10,15) `shouldBe` Just (10,15)
        it "overlap (10::Int,20) (15,25)" $ do
            overlap (10::Int,20) (15,25) `shouldBe` Just (15,20)
        prop "overlap test by QuickCheck" test_overlap


test_overlap :: (Integer, Integer) -> (Integer, Integer) -> Bool
test_overlap a b = let m = overlap a b
                   in
                     case m of
                       Nothing -> True
                       Just (la,lb) -> la <= lb
