module Main exposing (main)

import Browser
import Debug
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Set exposing (..)



-- TYPES


type alias Model =
    { email : Bool
    , autoplay : Autoplay
    , location : Bool
    , fruits : List String
    , selected : Set String
    }


type Autoplay
    = Off AutoplaySettings
    | On AutoplaySettings


type alias AutoplaySettings =
    { audio : Bool, withoutwifi : Bool }


type Field
    = Checkbox { checked : Bool, id : String }



-- Func


isAutoplay autoplay =
    case autoplay of
        On _ ->
            True

        Off _ ->
            False


getAutoplay autoplay =
    case autoplay of
        On a ->
            a

        Off a ->
            a


model =
    { email = True
    , autoplay = On { audio = True, withoutwifi = False }
    , location = True
    }


withAutoplayChildren =
    [ Checkbox { checked = model.email, id = "email" }
    , Checkbox { checked = isAutoplay model.autoplay, id = "autoplay" }
    , Checkbox { checked = (getAutoplay model.autoplay).audio, id = "audio" }
    , Checkbox { checked = (getAutoplay model.autoplay).withoutwifi, id = "withoutwifi" }
    , Checkbox { checked = model.location, id = "location" }
    ]


withOutAutoplayChildren =
    [ Checkbox { checked = model.email, id = "email" }
    , Checkbox { checked = isAutoplay model.autoplay, id = "autoplay" }
    , Checkbox { checked = model.location, id = "location" }
    ]


main =
    text <| Debug.toString model
