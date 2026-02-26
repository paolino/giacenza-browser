module View.Results where

import Prelude
import Compute (aggregateResults)
import Data.Array as Array
import Data.Map as Map
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Types
  ( Giacenza(..)
  , Result
  , Saldo(..)
  , Value(..)
  , Year(..)
  , YearResult
  , unValue
  )

-- | Render a single result table
renderResultTable
  :: forall w i. Result -> HH.HTML w i
renderResultTable result =
  if Map.isEmpty result then
    HH.p_ [ HH.text "No results." ]
  else
    HH.table
      [ HP.class_ (H.ClassName "result-table") ]
      [ HH.thead_
          [ HH.tr_
              [ HH.th_ [ HH.text "Year" ]
              , HH.th_ [ HH.text "Giacenza Media" ]
              , HH.th_ [ HH.text "Saldo" ]
              ]
          ]
      , HH.tbody_
          (map renderRow (Map.toUnfoldable result))
      ]
  where
  renderRow (Tuple (Year y) yr) =
    HH.tr_
      [ HH.td_ [ HH.text (show y) ]
      , HH.td
          [ HP.class_ (H.ClassName "number") ]
          [ HH.text
              (formatValue (unGiacenza yr.giacenza))
          ]
      , HH.td
          [ HP.class_ (H.ClassName "number") ]
          [ HH.text
              (formatValue (unSaldo yr.saldo))
          ]
      ]

  unGiacenza (Giacenza v) = v
  unSaldo (Saldo v) = v

-- | Render aggregated results across files
renderAggregated
  :: forall w i. Array Result -> HH.HTML w i
renderAggregated results =
  if Array.length results < 2 then
    HH.text ""
  else
    let
      total = aggregateResults results
    in
      HH.div
        [ HP.class_ (H.ClassName "aggregated") ]
        [ HH.h2_ [ HH.text "Aggregated Results" ]
        , renderResultTable total
        ]

-- | Format a Value to 2 decimal places
formatValue :: Value -> String
formatValue (Value n) = toStringWith (fixed 2) n
