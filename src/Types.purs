module Types where

import Prelude
import Data.Map (Map)
import Data.Maybe (Maybe)

-- | Monetary amount
newtype Value = Value Number

derive newtype instance eqValue :: Eq Value
derive newtype instance ordValue :: Ord Value
derive newtype instance semiringValue :: Semiring Value
derive newtype instance ringValue :: Ring Value

unValue :: Value -> Number
unValue (Value v) = v

-- | Calendar year
newtype Year = Year Int

derive newtype instance eqYear :: Eq Year
derive newtype instance ordYear :: Ord Year

unYear :: Year -> Int
unYear (Year y) = y

-- | End-of-year balance
newtype Saldo = Saldo Value

unSaldo :: Saldo -> Value
unSaldo (Saldo v) = v

-- | Average daily balance
newtype Giacenza = Giacenza Value

unGiacenza :: Giacenza -> Value
unGiacenza (Giacenza v) = v

-- | A single transaction
type Movement =
  { date :: SimpleDate
  , amount :: Value
  }

-- | Simple date representation (no dependency on js Date)
type SimpleDate =
  { year :: Int
  , month :: Int
  , day :: Int
  }

-- | Number format for parsing amounts
data NumberFormat
  = European -- 1.234,56
  | American -- 1,234.56

derive instance eqNumberFormat :: Eq NumberFormat

-- | Per-file configuration
type Config =
  { numberFormat :: NumberFormat
  , dateColumn :: String
  , amountColumn :: String
  }

-- | Per-year result
type YearResult =
  { saldo :: Saldo
  , giacenza :: Giacenza
  }

-- | Full result: per-year map
type Result = Map Year YearResult

-- | State of a loaded file
data FileState
  = NotConfigured (Array String) -- headers detected
  | Configured Config (Array String) -- config + headers
  | Success Config Result
  | Failed Config String

-- | A loaded file with its content and state
type LoadedFile =
  { name :: String
  , content :: String
  , state :: FileState
  }

-- | Application state
type AppState =
  { files :: Array LoadedFile
  , darkTheme :: Boolean
  , lastConfig :: Maybe Config
  }
