module Jabara.UtilSpec (spec) where

import Test.Hspec

import Jabara.Util

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
