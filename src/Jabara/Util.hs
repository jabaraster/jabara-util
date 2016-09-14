{-# LANGUAGE OverloadedStrings #-}
module Jabara.Util (
    overlap
  , comma
  , commaS
  , listToMap
  , listToListMap
  , toFirstCharLower
  , omittedFirstCharLower
  , parseDataUrlData
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64 (decode)
import           Data.Char (toLower)
import           Data.Either (Either(..))
import           Data.List (reverse, concat, zip, cycle)
import qualified Data.Map as Map (Map, empty, singleton, insert, insertWith)
import qualified Data.Text as T (Text, splitOn)
import qualified Data.Text.Encoding as T (encodeUtf8)
import           Data.Tuple (fst, snd)
import           GHC.Base
import           GHC.Show (Show(..))
import           Prelude ((!!), head, drop, length)

comma :: String -> String
comma s = concat $ reverse [[n]++c|(c,n)<- zip ("":(cycle ["","",","]))  (reverse s)]

commaS :: Show a => a -> String
commaS = comma . show

listToMap :: Ord k => (a -> k) -> [a] -> Map.Map k a
listToMap _ []     = Map.empty
listToMap f [a]    = Map.singleton (f a) a
listToMap f (a:as) = Map.insert (f a) a $ listToMap f as

listToListMap :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
listToListMap _ []     = Map.empty
listToListMap f [a]    = Map.singleton (f a) [a]
listToListMap f (a:as) = Map.insertWith (++) (f a) [a] $ listToListMap f as

toFirstCharLower :: String -> String
toFirstCharLower "" = ""
toFirstCharLower s = (toLower $ head s):(drop 1 s)

type AccessorName = String
type AccessorPrefix = String
omittedFirstCharLower :: AccessorPrefix -> AccessorName -> String
omittedFirstCharLower prefix = toFirstCharLower . drop (length prefix)

overlap :: Ord a => (a, a) -> (a, a) -> Maybe (a, a)
overlap a1 a2 = let na1 = n a1
                    na2 = n a2
                in
                    if fst na1 <= fst na2 then core na1 na2 >>= Just . n
                      else core na2 na1 >>= Just . n
  where
    n :: Ord a => (a, a) -> (a, a) -- 大小関係を整える
    n r@(f, s) | f < s     = r
               | otherwise = (s, f)

    core :: Ord a => (a, a) -> (a, a) -> Maybe (a, a)
    core (r1Start, r1End) r2@(r2Start, r2End)
        -- r1Start <= r2Startが保証されている

        --  r1                       r1
        --   |------------------------>
        --        r2           r2
        --         |------------>
        | r2End <= r1End = Just r2
        -- r1         r1
        --  |---------->
        --            r2         r2
        --             |---------->
        | r1End == r2Start = Just (r1End, r1End)
        -- r1         r1
        --  |---------->
        --                 r2         r2
        --                  |---------->
        | r1End < r2Start = Nothing
        -- r1                r1
        --  |----------------->
        --          r2               r2
        --           |---------------->
        | r1Start <= r2Start = Just (r2Start, r1End)

        | otherwise = error "out of supposition."

parseDataUrlData :: Maybe T.Text -> Either String ByteString
parseDataUrlData mUrl = case mUrl of
                   Nothing    -> Left ""
                   Just image -> 
                     let
                         tokens = T.splitOn "base64," image
                     in
                         if length tokens < 2 then
                             Left ""
                           else
                             B64.decode $ T.encodeUtf8 (tokens!!1)

