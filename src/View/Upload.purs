module View.Upload where

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen as H
import Web.Event.Event (Event)

-- | Render the file upload area
renderUpload
  :: forall w. (Event -> w) -> H.ComponentHTML w () _
renderUpload onEvent =
  HH.div
    [ HP.class_ (H.ClassName "upload-area") ]
    [ HH.label
        [ HP.class_ (H.ClassName "upload-label") ]
        [ HH.input
            [ HP.type_ HP.InputFile
            , HP.multiple true
            , HP.class_ (H.ClassName "upload-input")
            , HE.onChange onEvent
            ]
        , HH.span_
            [ HH.text "Choose CSV files..." ]
        ]
    ]
