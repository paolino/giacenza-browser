module Main where

import Prelude
import Compute (aggregateResults, compute)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import FFI.FileReader
  ( fileName
  , getFilesFromEvent
  , readFile
  )
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Parse (extractMovements, parseCSV)
import Types
  ( AppState
  , Config
  , FileState(..)
  , LoadedFile
  , NumberFormat(..)
  , Result
  )
import View.FileList (renderFileList)
import View.Results (renderAggregated)
import View.Upload (renderUpload)
import Web.Event.Event (Event)
import Web.File.File (File)

data Action
  = Initialize
  | FileInputChanged Event
  | Configure Int Config
  | PropagateConfig Config
  | Analyze Int
  | Reconfigure Int
  | RemoveFile Int
  | ClearAll
  | ToggleTheme

type State = AppState

initialState :: State
initialState =
  { files: []
  , darkTheme: true
  , lastConfig: Nothing
  }

rootComponent
  :: forall q i o m
   . MonadAff m
  => H.Component q i o m
rootComponent =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }

render
  :: forall m
   . State
  -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (H.ClassName "app") ]
    [ renderHeader state
    , HH.div
        [ HP.class_ (H.ClassName "container") ]
        [ renderUpload FileInputChanged
        , renderFileList
            { files: state.files
            , onConfigure: Configure
            , onAnalyze: Analyze
            , onReconfigure: Reconfigure
            , onRemove: RemoveFile
            , onPropagate: PropagateConfig
            }
        , renderAggregated
            (getSuccessResults state)
        , renderFooter
        ]
    ]

renderHeader
  :: forall m
   . State
  -> H.ComponentHTML Action () m
renderHeader state =
  HH.header
    [ HP.class_ (H.ClassName "header") ]
    [ HH.h1_ [ HH.text "Giacenza Media" ]
    , HH.button
        [ HP.class_ (H.ClassName "theme-btn")
        , HE.onClick \_ -> ToggleTheme
        ]
        [ HH.text
            if state.darkTheme then "\x1F319"
            else "\x2600\xFE0F"
        ]
    ]

renderFooter :: forall w i. HH.HTML w i
renderFooter =
  HH.footer
    [ HP.class_ (H.ClassName "footer") ]
    [ HH.p_
        [ HH.text
            "Your data never leaves this browser."
        ]
    ]

handleAction
  :: forall o m
   . MonadAff m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> pure unit

  FileInputChanged event -> do
    files <- liftEffect
      (getFilesFromEvent event)
    loaded <- liftAff
      $ traverse loadOneFile files
    H.modify_ \s ->
      s { files = s.files <> loaded }

  Configure idx config -> do
    H.modify_ \s ->
      s
        { files = updateFileAt idx
            (configureFile config)
            s.files
        , lastConfig = Just config
        }
    analyzeFile idx

  PropagateConfig config -> do
    H.modify_ \s ->
      s
        { files = map
            (propagateToUnconfigured config)
            s.files
        , lastConfig = Just config
        }
    state <- H.get
    let
      indexed = Array.mapWithIndex
        (\i f -> { i, f })
        state.files
      toAnalyze = Array.filter
        (\x -> isConfigured x.f.state)
        indexed
    void $ traverse
      (\x -> analyzeFile x.i)
      toAnalyze

  Analyze idx -> analyzeFile idx

  Reconfigure idx ->
    H.modify_ \s ->
      s
        { files = updateFileAt idx
            reconfigureFile
            s.files
        }

  RemoveFile idx ->
    H.modify_ \s ->
      s { files = removeAt idx s.files }

  ClearAll ->
    H.modify_ _ { files = [] }

  ToggleTheme ->
    H.modify_ \s ->
      s { darkTheme = not s.darkTheme }

-- | Load a single File into memory (in Aff)
loadOneFile :: File -> Aff LoadedFile
loadOneFile file = do
  content <- readFile file
  let
    csv = parseCSV content
    headers = csv.headers
  pure
    { name: fileName file
    , content
    , state: NotConfigured headers
    }

-- | Run analysis on file at index
analyzeFile
  :: forall o m
   . MonadAff m
  => Int
  -> H.HalogenM State Action () o m Unit
analyzeFile idx = do
  state <- H.get
  case Array.index state.files idx of
    Nothing -> pure unit
    Just file -> case file.state of
      Configured config _ -> do
        let
          csv = parseCSV file.content
          result = extractMovements
            { dateCol: config.dateColumn
            , amountCol: config.amountColumn
            , fmt: config.numberFormat
            }
            csv
        case result of
          Left err ->
            H.modify_ \s ->
              s
                { files = updateFileAt idx
                    ( \f ->
                        f
                          { state =
                              Failed config err
                          }
                    )
                    s.files
                }
          Right movements ->
            let
              computed = compute movements
            in
              H.modify_ \s ->
                s
                  { files = updateFileAt idx
                      ( \f ->
                          f
                            { state =
                                Success config
                                  computed
                            }
                      )
                      s.files
                  }
      _ -> pure unit

-- | Update file at index
updateFileAt
  :: Int
  -> (LoadedFile -> LoadedFile)
  -> Array LoadedFile
  -> Array LoadedFile
updateFileAt idx f files =
  Array.mapWithIndex
    ( \i file ->
        if i == idx then f file else file
    )
    files

-- | Remove file at index
removeAt :: forall a. Int -> Array a -> Array a
removeAt idx arr =
  Array.mapWithIndex (\i a -> { i, a }) arr
    # Array.filter (\x -> x.i /= idx)
    # map _.a

-- | Set file to Configured state
configureFile
  :: Config -> LoadedFile -> LoadedFile
configureFile config file = case file.state of
  NotConfigured headers ->
    file { state = Configured config headers }
  Failed _ _ ->
    let
      csv = parseCSV file.content
    in
      file
        { state = Configured config csv.headers
        }
  _ -> file

-- | Return file to NotConfigured
reconfigureFile :: LoadedFile -> LoadedFile
reconfigureFile file =
  let
    csv = parseCSV file.content
  in
    file { state = NotConfigured csv.headers }

-- | Apply config to unconfigured files
propagateToUnconfigured
  :: Config -> LoadedFile -> LoadedFile
propagateToUnconfigured config file =
  case file.state of
    NotConfigured headers ->
      file
        { state = Configured config headers }
    _ -> file

-- | Check if file is in Configured state
isConfigured :: FileState -> Boolean
isConfigured (Configured _ _) = true
isConfigured _ = false

-- | Get all successful results
getSuccessResults :: State -> Array Result
getSuccessResults state =
  Array.mapMaybe getResult state.files
  where
  getResult file = case file.state of
    Success _ result -> Just result
    _ -> Nothing

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI rootComponent unit body
