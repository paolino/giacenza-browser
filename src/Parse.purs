module Parse where

import Prelude
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String as S
import Data.String.CodePoints as SCP
import Data.String.CodeUnits as SCU
import Types
  ( Movement
  , NumberFormat(..)
  , SimpleDate
  , Value(..)
  )

-- | Parse a CSV string into headers and rows
parseCSV
  :: String
  -> { headers :: Array String
     , rows :: Array (Array String)
     }
parseCSV input =
  let
    lines = Array.filter
      (not <<< S.null <<< S.trim)
      $ S.split (S.Pattern "\n")
          (normalizeLineEndings input)
  in
    case Array.uncons lines of
      Nothing -> { headers: [], rows: [] }
      Just { head: headerLine, tail: dataLines } ->
        { headers:
            map S.trim (splitCSVLine headerLine)
        , rows: map splitCSVLine dataLines
        }

-- | Normalize \r\n to \n
normalizeLineEndings :: String -> String
normalizeLineEndings =
  S.replaceAll (S.Pattern "\r\n") (S.Replacement "\n")
    >>> S.replaceAll
      (S.Pattern "\r")
      (S.Replacement "\n")

-- | Split a CSV line respecting quoted fields
splitCSVLine :: String -> Array String
splitCSVLine line =
  splitFields (SCU.toCharArray line) [] "" false
  where
  splitFields chars acc current inQuotes =
    case Array.uncons chars of
      Nothing -> Array.snoc acc (S.trim current)
      Just { head: c, tail: rest }
        | c == '"' ->
            splitFields rest acc current
              (not inQuotes)
        | c == ',' && not inQuotes ->
            splitFields rest
              (Array.snoc acc (S.trim current))
              ""
              false
        | c == ';' && not inQuotes ->
            splitFields rest
              (Array.snoc acc (S.trim current))
              ""
              false
        | otherwise ->
            splitFields rest acc
              (current <> SCU.singleton c)
              inQuotes

-- | Parse a date string in YYYY-MM-DD format
parseDate :: String -> Maybe SimpleDate
parseDate s =
  case S.split (S.Pattern "-") s of
    [ yStr, mStr, dStr ] -> do
      y <- Int.fromString yStr
      m <- Int.fromString mStr
      d <- Int.fromString dStr
      if m >= 1 && m <= 12 && d >= 1 && d <= 31 then
        Just { year: y, month: m, day: d }
      else
        Nothing
    _ -> Nothing

-- | Parse a monetary value with the given format
parseValue
  :: NumberFormat -> String -> Either String Value
parseValue fmt raw =
  let
    trimmed = S.trim raw
    { negative, rest } = parseSign trimmed
    { decSep, thousSep } = separators fmt
    parts = S.split
      (S.Pattern (SCU.singleton thousSep))
      rest
  in
    case parseDigitGroups decSep parts of
      Nothing -> Left ("Invalid number: " <> raw)
      Just { euros, cents } ->
        Right $ Value
          $ applySign negative
          $ Int.toNumber euros + cents

-- | Get separator chars for a number format
separators
  :: NumberFormat
  -> { decSep :: Char, thousSep :: Char }
separators European =
  { decSep: ',', thousSep: '.' }
separators American =
  { decSep: '.', thousSep: ',' }

parseSign
  :: String
  -> { negative :: Boolean, rest :: String }
parseSign s = case SCU.charAt 0 s of
  Just '-' ->
    { negative: true, rest: SCU.drop 1 s }
  Just '+' ->
    { negative: false, rest: SCU.drop 1 s }
  _ -> { negative: false, rest: s }

applySign :: Boolean -> Number -> Number
applySign true n = negate n
applySign false n = n

-- | Parse digit groups and extract decimal part
parseDigitGroups
  :: Char
  -> Array String
  -> Maybe { euros :: Int, cents :: Number }
parseDigitGroups decSep parts =
  let
    cleaned = map
      ( S.replaceAll (S.Pattern " ")
          (S.Replacement "")
      )
      parts
  in
    case Array.unsnoc cleaned of
      Nothing -> Nothing
      Just { init: initParts, last: lastPart } ->
        let
          { intPart, decPart } =
            splitDecimal decSep lastPart
          allIntParts =
            Array.snoc initParts intPart
        in
          case foldIntParts allIntParts of
            Nothing -> Nothing
            Just euros ->
              let
                cents = case decPart of
                  Nothing -> 0.0
                  Just dp -> case Int.fromString dp of
                    Nothing -> 0.0
                    Just c ->
                      Int.toNumber c
                        / Int.toNumber
                            (pow10 (S.length dp))
              in
                Just { euros, cents }

foldIntParts :: Array String -> Maybe Int
foldIntParts arr = case Array.uncons arr of
  Nothing -> Nothing
  Just { head: first, tail: rest } ->
    case Int.fromString first of
      Nothing ->
        if S.null first then
          foldl accum (Just 0) rest
        else Nothing
      Just n -> foldl accum (Just n) rest
  where
  accum acc part = do
    a <- acc
    p <- Int.fromString part
    Just (a * 1000 + p)

-- | Split a string at the decimal separator
splitDecimal
  :: Char
  -> String
  -> { intPart :: String, decPart :: Maybe String }
splitDecimal sep s =
  case S.indexOf
    (S.Pattern (SCU.singleton sep))
    s of
    Nothing -> { intPart: s, decPart: Nothing }
    Just idx ->
      { intPart: S.take idx s
      , decPart: Just (S.drop (idx + 1) s)
      }

pow10 :: Int -> Int
pow10 0 = 1
pow10 n = 10 * pow10 (n - 1)

-- | Extract movements from parsed CSV
extractMovements
  :: { dateCol :: String
     , amountCol :: String
     , fmt :: NumberFormat
     }
  -> { headers :: Array String
     , rows :: Array (Array String)
     }
  -> Either String (Array Movement)
extractMovements cfg csv =
  case findColumnIndex cfg.dateCol csv.headers
    , findColumnIndex cfg.amountCol csv.headers of
    Nothing, _ ->
      Left
        ( "Date column not found: "
            <> cfg.dateCol
        )
    _, Nothing ->
      Left
        ( "Amount column not found: "
            <> cfg.amountCol
        )
    Just dateIdx, Just amountIdx ->
      parseRows dateIdx amountIdx cfg.fmt
        csv.rows

findColumnIndex
  :: String -> Array String -> Maybe Int
findColumnIndex name headers =
  Array.findIndex (_ == name) headers

parseRows
  :: Int
  -> Int
  -> NumberFormat
  -> Array (Array String)
  -> Either String (Array Movement)
parseRows dateIdx amountIdx fmt rows =
  foldl parseRow (Right [])
    ( Array.mapWithIndex
        (\i r -> { idx: i, row: r })
        rows
    )
  where
  parseRow acc { idx: rowIdx, row } =
    case acc of
      Left err -> Left err
      Right movements ->
        case Array.index row dateIdx
          , Array.index row amountIdx of
          Just dateStr, Just amountStr ->
            case parseDate (S.trim dateStr) of
              Nothing ->
                Left
                  ( "Invalid date at row "
                      <> show (rowIdx + 2)
                      <> ": "
                      <> dateStr
                  )
              Just d ->
                case parseValue fmt amountStr of
                  Left err ->
                    Left
                      ( "Row "
                          <> show (rowIdx + 2)
                          <> ": "
                          <> err
                      )
                  Right v ->
                    Right
                      $ Array.snoc movements
                          { date: d, amount: v }
          _, _ ->
            Left
              ( "Missing columns at row "
                  <> show (rowIdx + 2)
              )
