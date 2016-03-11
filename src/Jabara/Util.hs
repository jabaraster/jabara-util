module Jabara.Util (
    overlap
    , comma
    , commaS
    , toFirstCharLower
) where

import Data.Char (toLower)
import Data.List (reverse, concat, zip, cycle)
import Data.Tuple (fst, snd)
import GHC.Base
import GHC.Show (Show(..))
import Prelude (head, drop)

comma :: String -> String
comma s = concat $ reverse [[n]++c|(c,n)<- zip ("":(cycle ["","",","]))  (reverse s)]

commaS :: Show a => a -> String
commaS = comma . show

toFirstCharLower :: String -> String
toFirstCharLower "" = ""
toFirstCharLower s = (toLower $ head s):(drop 1 s)

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

