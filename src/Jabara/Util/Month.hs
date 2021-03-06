module Jabara.Util.Month (
    Month(mDay)
  , month
  , monthFromDay
  , separateMonth
  , dayFromMonth
  , dayToMonth
  , addMonth
  , isMarch
  , toFiscalYear
  , toSchoolYear
  , monthsToEndOfSchoolYear
) where

import           Data.List          (sort)
import           Data.Time.Calendar
import           Text.Read          (readMaybe)

newtype Month = Month { mDay :: Day }
    deriving (Eq, Ord)
instance Show Month where
    show month = let (y, m) = separateMonth month
                 in
                     if m < 10 then (show y) ++ "/0" ++ (show m)
                               else (show y) ++ "/"  ++ (show m)
instance Read Month where
    readsPrec _ ""       = []
    readsPrec _ s
         | length s /= 7 = []
         | otherwise     = let ms = take 4 s
                               ds = drop 5 s
                           in  case (readMaybe ms, readMaybe ds) of
                               (Nothing, _)     -> []
                               (_, Nothing)     -> []
                               (Just m, Just d) -> [(month m d, "")]

instance Enum Month where
    fromEnum mnt = let (y, m) = separateMonth mnt
                   in  fromInteger (y * 100 + (toInteger m))
    toEnum   val = let year = floor ((realToFrac val) / 100)
                       m    = (toInteger val) - (year * 100)
                   in  month year (fromInteger m)

month :: Integer -> Int -> Month
month y m = Month $ fromGregorian y m 1

separateMonth :: Month -> (Integer, Int)
separateMonth month = let (y, m, _) = toGregorian $ mDay month
                      in  (y, m)

dayToMonth :: Day -> Month
dayToMonth d = Month d

monthFromDay :: Day -> Month
monthFromDay d = case toGregorian d of (y, m, _) -> month y m

addMonth :: Integer -> Month -> Month
addMonth i m = Month $ addGregorianMonthsClip i $ mDay m

isMarch :: Month -> Bool
isMarch m = case toGregorian $ mDay m of
                  (_, 3, _) -> True
                  _         -> False

dayFromMonth :: Month -> Day
dayFromMonth = mDay

toSchoolYear :: Month -> Integer
toSchoolYear m = case toGregorian $ mDay m of
                   (y, m, _) | m <= 3    -> y - 1
                             | otherwise -> y

toFiscalYear :: Month -> Integer
toFiscalYear = toSchoolYear

monthsToEndOfSchoolYear :: Month -> [Month]
monthsToEndOfSchoolYear startMonth = core 0 []
  where
    core :: Integer -> [Month] -> [Month]
    core offset ms = let m = addMonth offset startMonth
                      in
                        if isMarch m then
                            sort (m:ms)
                          else
                            core (offset+1) (m:ms)
