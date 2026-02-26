module View.FileList where

import Prelude
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types
  ( Config
  , FileState(..)
  , Giacenza(..)
  , LoadedFile
  , NumberFormat(..)
  , Result
  , Saldo(..)
  , Value(..)
  , Year(..)
  , YearResult
  , unValue
  )
import View.Results (renderResultTable)

type FileListProps w =
  { files :: Array LoadedFile
  , onConfigure :: Int -> Config -> w
  , onAnalyze :: Int -> w
  , onReconfigure :: Int -> w
  , onRemove :: Int -> w
  , onPropagate :: Config -> w
  }

renderFileList
  :: forall w. FileListProps w -> H.ComponentHTML w () _
renderFileList props =
  if Array.null props.files then HH.text ""
  else
    HH.div
      [ HP.class_ (H.ClassName "file-list") ]
      ( Array.mapWithIndex
          (renderFileCard props)
          props.files
      )

renderFileCard
  :: forall w
   . FileListProps w
  -> Int
  -> LoadedFile
  -> H.ComponentHTML w () _
renderFileCard props idx file =
  HH.div
    [ HP.class_ (H.ClassName "file-card") ]
    [ HH.div
        [ HP.class_ (H.ClassName "file-header") ]
        [ HH.span
            [ HP.class_ (H.ClassName "file-name") ]
            [ HH.text file.name ]
        , renderBadge file.state
        , HH.button
            [ HP.class_ (H.ClassName "btn btn-danger btn-sm")
            , HE.onClick \_ -> props.onRemove idx
            ]
            [ HH.text "Remove" ]
        ]
    , renderFileBody props idx file
    ]

renderBadge
  :: forall w i. FileState -> HH.HTML w i
renderBadge state =
  HH.span [ HP.class_ (H.ClassName cls) ]
    [ HH.text label ]
  where
  cls = "badge " <> case state of
    NotConfigured _ -> "badge-pending"
    Configured _ _ -> "badge-config"
    Success _ _ -> "badge-success"
    Failed _ _ -> "badge-error"

  label = case state of
    NotConfigured _ -> "Not configured"
    Configured _ _ -> "Ready"
    Success _ _ -> "Success"
    Failed _ _ -> "Error"

renderFileBody
  :: forall w
   . FileListProps w
  -> Int
  -> LoadedFile
  -> H.ComponentHTML w () _
renderFileBody props idx file = case file.state of
  NotConfigured headers ->
    renderConfigForm props idx headers Nothing

  Configured config headers ->
    HH.div_
      [ HH.p_ [ HH.text "Analyzing..." ]
      ]

  Success config result ->
    HH.div_
      [ renderConfigSummary config
      , renderResultTable result
      , HH.button
          [ HP.class_
              (H.ClassName "btn btn-secondary")
          , HE.onClick \_ -> props.onReconfigure idx
          ]
          [ HH.text "Reconfigure" ]
      , HH.button
          [ HP.class_
              (H.ClassName "btn btn-primary")
          , HE.onClick \_ ->
              props.onPropagate config
          ]
          [ HH.text "Apply to all" ]
      ]

  Failed config err ->
    HH.div_
      [ HH.p
          [ HP.class_ (H.ClassName "error") ]
          [ HH.text err ]
      , renderConfigSummary config
      , HH.button
          [ HP.class_
              (H.ClassName "btn btn-secondary")
          , HE.onClick \_ -> props.onReconfigure idx
          ]
          [ HH.text "Reconfigure" ]
      ]

renderConfigSummary
  :: forall w i. Config -> HH.HTML w i
renderConfigSummary config =
  HH.div
    [ HP.class_ (H.ClassName "config-summary") ]
    [ HH.text
        ( "Date: "
            <> config.dateColumn
            <> " | Amount: "
            <> config.amountColumn
            <> " | Format: "
            <> formatLabel config.numberFormat
        )
    ]

renderConfigForm
  :: forall w
   . FileListProps w
  -> Int
  -> Array String
  -> Maybe Config
  -> H.ComponentHTML w () _
renderConfigForm props idx headers mConfig =
  HH.form
    [ HP.class_ (H.ClassName "config-form")
    , HP.id ("config-form-" <> show idx)
    ]
    [ HH.div
        [ HP.class_ (H.ClassName "form-group") ]
        [ HH.label_ [ HH.text "Date column" ]
        , HH.select
            [ HP.class_ (H.ClassName "form-control")
            , HP.id
                ("date-col-" <> show idx)
            ]
            (map mkOption headers)
        ]
    , HH.div
        [ HP.class_ (H.ClassName "form-group") ]
        [ HH.label_ [ HH.text "Amount column" ]
        , HH.select
            [ HP.class_ (H.ClassName "form-control")
            , HP.id
                ("amount-col-" <> show idx)
            ]
            (map mkOption headers)
        ]
    , HH.div
        [ HP.class_ (H.ClassName "form-group") ]
        [ HH.label_ [ HH.text "Number format" ]
        , HH.div
            [ HP.class_ (H.ClassName "radio-group") ]
            [ HH.label_
                [ HH.input
                    [ HP.type_ HP.InputRadio
                    , HP.name
                        ("fmt-" <> show idx)
                    , HP.value "european"
                    , HP.checked true
                    ]
                , HH.text " European (1.234,56)"
                ]
            , HH.label_
                [ HH.input
                    [ HP.type_ HP.InputRadio
                    , HP.name
                        ("fmt-" <> show idx)
                    , HP.value "american"
                    ]
                , HH.text " American (1,234.56)"
                ]
            ]
        ]
    , HH.button
        [ HP.type_ HP.ButtonButton
        , HP.class_ (H.ClassName "btn btn-primary")
        , HE.onClick \_ ->
            props.onConfigure idx defaultConfig
        ]
        [ HH.text "Analyze" ]
    ]
  where
  mkOption h =
    HH.option [ HP.value h ] [ HH.text h ]

  defaultConfig =
    { numberFormat: European
    , dateColumn: fromMaybe "" (Array.head headers)
    , amountColumn:
        fromMaybe ""
          (Array.index headers 1)
    }

formatLabel :: NumberFormat -> String
formatLabel European = "European"
formatLabel American = "American"
