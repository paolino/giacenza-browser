module Compute where

import Prelude
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Types
  ( Giacenza(..)
  , Movement
  , Result
  , Saldo(..)
  , SimpleDate
  , Value(..)
  , Year(..)
  , YearResult
  , unValue
  )

-- | Main computation: movements -> per-year results
compute :: Array Movement -> Result
compute movements =
  let
    sorted = Array.sortWith _.date movements
    daily = foldDays sorted
    byYear = groupByYear daily
  in
    Map.mapMaybeWithKey computeYear byYear

-- | Expand sparse movements into dense daily balances
foldDays
  :: Array Movement
  -> Array { date :: SimpleDate, balance :: Value }
foldDays movements = case Array.uncons movements of
  Nothing -> []
  Just { head: first, tail: rest } ->
    let
      init =
        { lastDate: first.date
        , balance: first.amount
        , entries:
            [ { date: first.date
              , balance: first.amount
              }
            ]
        }
      result = foldl step init rest
      -- Fill from last movement to Dec 31
      endOfYear =
        { year: result.lastDate.year
        , month: 12
        , day: 31
        }
      finalFill = fillDays result.lastDate endOfYear
        result.balance
    in
      result.entries <> finalFill
  where
  step acc mov =
    let
      fill = fillDays acc.lastDate mov.date acc.balance
      newBalance = addValues acc.balance mov.amount
    in
      { lastDate: mov.date
      , balance: newBalance
      , entries: acc.entries <> fill <>
          [ { date: mov.date
            , balance: newBalance
            }
          ]
      }

-- | Fill days between two dates (exclusive start,
-- inclusive end) with a constant balance
fillDays
  :: SimpleDate
  -> SimpleDate
  -> Value
  -> Array { date :: SimpleDate, balance :: Value }
fillDays from to balance =
  let
    fromDayNum = dateToDayNumber from
    toDayNum = dateToDayNumber to
    days = Array.range (fromDayNum + 1) (toDayNum - 1)
  in
    map
      ( \d ->
          { date: dayNumberToDate from.year d
          , balance
          }
      )
      days

-- | Group daily entries by year
groupByYear
  :: Array { date :: SimpleDate, balance :: Value }
  -> Map.Map Year
       (Array { date :: SimpleDate, balance :: Value })
groupByYear entries =
  foldl
    ( \acc entry ->
        let
          y = Year entry.date.year
        in
          Map.insertWith (<>) y [ entry ] acc
    )
    Map.empty
    entries

-- | Compute saldo and giacenza for a single year
computeYear
  :: Year
  -> Array { date :: SimpleDate, balance :: Value }
  -> Maybe YearResult
computeYear (Year y) entries =
  let
    daysInYear = if isLeapYear y then 366 else 365
    -- Find Dec 31 entry for saldo
    dec31 = Array.find
      ( \e ->
          e.date.month == 12 && e.date.day == 31
      )
      entries
    saldo = map (_.balance >>> Saldo) dec31
    -- Compute giacenza = sum of daily balances / days
    totalBalance = foldl
      (\acc e -> addValues acc e.balance)
      (Value 0.0)
      entries
    nDays = Array.length entries
    giacenza =
      if nDays > 0 then
        Just $ Giacenza $ Value
          $ unValue totalBalance
              / Int.toNumber daysInYear
      else
        Nothing
  in
    case saldo, giacenza of
      Just s, Just g -> Just { saldo: s, giacenza: g }
      _, _ -> Nothing

-- | Aggregate results from multiple files
aggregateResults :: Array Result -> Result
aggregateResults results =
  foldl mergeResults Map.empty results
  where
  mergeResults acc r =
    Map.unionWith addYearResults acc r

  addYearResults a b =
    { saldo: addSaldos a.saldo b.saldo
    , giacenza:
        addGiacenzas a.giacenza b.giacenza
    }

  addSaldos (Saldo (Value a)) (Saldo (Value b)) =
    Saldo (Value (a + b))

  addGiacenzas
    (Giacenza (Value a))
    (Giacenza (Value b)) =
    Giacenza (Value (a + b))

-- | Add two Values
addValues :: Value -> Value -> Value
addValues (Value a) (Value b) = Value (a + b)

-- | Check if a year is a leap year
isLeapYear :: Int -> Boolean
isLeapYear y =
  (y `mod` 4 == 0 && y `mod` 100 /= 0)
    || y `mod` 400 == 0

-- | Convert date to day-of-year number (1-366)
dateToDayNumber :: SimpleDate -> Int
dateToDayNumber d =
  let
    monthDays = daysBeforeMonth d.year d.month
  in
    monthDays + d.day

-- | Days before the start of a given month
daysBeforeMonth :: Int -> Int -> Int
daysBeforeMonth year month =
  let
    base = case month of
      1 -> 0
      2 -> 31
      3 -> 59
      4 -> 90
      5 -> 120
      6 -> 151
      7 -> 181
      8 -> 212
      9 -> 243
      10 -> 273
      11 -> 304
      12 -> 334
      _ -> 0
    leap =
      if month > 2 && isLeapYear year then 1
      else 0
  in
    base + leap

-- | Convert day-of-year number back to date
dayNumberToDate :: Int -> Int -> SimpleDate
dayNumberToDate year dayNum =
  let
    leap = isLeapYear year
    monthLengths =
      [ 31
      , if leap then 29 else 28
      , 31
      , 30
      , 31
      , 30
      , 31
      , 31
      , 30
      , 31
      , 30
      , 31
      ]
    result = foldl findMonth
      { remaining: dayNum, month: 1, found: false }
      monthLengths
  in
    { year
    , month: result.month
    , day: result.remaining
    }
  where
  findMonth acc days =
    if acc.found || acc.remaining <= days then
      acc { found = true }
    else
      { remaining: acc.remaining - days
      , month: acc.month + 1
      , found: false
      }
