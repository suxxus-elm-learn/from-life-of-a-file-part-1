module Main exposing (main)

import Browser
import Html exposing (Html, input, li, text, ul)
import Html.Attributes exposing (checked, id, placeholder, type_)
import Html.Events exposing (onClick)


type Field
    = Email
    | Location
    | Video
    | Audio
    | Wifi


type AutoplayCtrls
    = AudioCtrl
    | WithOutWifiCtrl


type Autoplay
    = Off AutoplaySettings
    | On AutoplaySettings


type Msg
    = ToggleEmail
    | ToggleLocation
    | ToggleAutoplay
    | ToggleAutoplayCtrls AutoplayCtrls


type alias Checkbox =
    { checked : Bool
    , tag : String
    , id : Field
    , onClick : Msg
    }


type alias Model =
    { emailNotifications : Bool
    , videoAutoplay : Autoplay
    , useLocation : Bool
    }


type alias AutoplaySettings =
    { audio : Bool
    , withoutwifi : Bool
    }


init : Model
init =
    { emailNotifications = False
    , videoAutoplay = On { audio = False, withoutwifi = False }
    , useLocation = False
    }


isVideoAutoplayOn : Model -> Bool
isVideoAutoplayOn { videoAutoplay } =
    case videoAutoplay of
        On _ ->
            True

        Off _ ->
            False


getAutoplaySettings : Autoplay -> AutoplaySettings
getAutoplaySettings autoplay =
    case autoplay of
        On ctrls ->
            ctrls

        Off ctrls ->
            ctrls


getAutoplayCtrlValue : AutoplayCtrls -> AutoplaySettings -> Bool
getAutoplayCtrlValue autoplayctrl autoplaySettings =
    case autoplayctrl of
        AudioCtrl ->
            autoplaySettings.audio

        WithOutWifiCtrl ->
            autoplaySettings.withoutwifi


getStatusOfAutoplayCtrl : Model -> AutoplayCtrls -> Bool
getStatusOfAutoplayCtrl { videoAutoplay } ctrl =
    getAutoplaySettings videoAutoplay
        |> getAutoplayCtrlValue ctrl


toggleAutoplay : Model -> Autoplay
toggleAutoplay { videoAutoplay } =
    case videoAutoplay of
        On ctrls ->
            Off ctrls

        Off ctrls ->
            On ctrls


toggleAutoplayCtrls : Model -> AutoplayCtrls -> Autoplay
toggleAutoplayCtrls model ctrls =
    let
        ctrlsSetting =
            case ctrls of
                AudioCtrl ->
                    { audio = not (getStatusOfAutoplayCtrl model AudioCtrl)
                    , withoutwifi = getStatusOfAutoplayCtrl model WithOutWifiCtrl
                    }

                WithOutWifiCtrl ->
                    { audio = getStatusOfAutoplayCtrl model AudioCtrl
                    , withoutwifi = not (getStatusOfAutoplayCtrl model WithOutWifiCtrl)
                    }

        videoAutoplay =
            case model.videoAutoplay of
                Off _ ->
                    Off ctrlsSetting

                On _ ->
                    On ctrlsSetting
    in
    videoAutoplay


listOfOptions : Model -> List Checkbox
listOfOptions model =
    [ { checked = model.emailNotifications
      , tag = "Email Notifications"
      , id = Email
      , onClick = ToggleEmail
      }
    , { checked = isVideoAutoplayOn model
      , tag = "Video autoplay"
      , id = Video
      , onClick = ToggleAutoplay
      }
    , { checked =
            getStatusOfAutoplayCtrl model AudioCtrl
      , tag = "Audio"
      , id = Audio
      , onClick = ToggleAutoplayCtrls AudioCtrl
      }
    , { checked =
            getStatusOfAutoplayCtrl model WithOutWifiCtrl
      , tag = "WithOutWifi"
      , id = Wifi
      , onClick = ToggleAutoplayCtrls WithOutWifiCtrl
      }
    , { checked = model.useLocation
      , tag = "Use location"
      , id = Location
      , onClick = ToggleLocation
      }
    ]


dropAutoPlaySettings : List Checkbox -> List Checkbox
dropAutoPlaySettings =
    List.filter
        (\checkbox ->
            not
                (List.member
                    checkbox.id
                    [ Audio
                    , Wifi
                    ]
                )
        )


tellUserOptions : Model -> List (Html Msg)
tellUserOptions model =
    if isVideoAutoplayOn model then
        listOfOptions model |> doHtmlCheckbox

    else
        dropAutoPlaySettings (listOfOptions model) |> doHtmlCheckbox


doHtmlCheckbox : List Checkbox -> List (Html Msg)
doHtmlCheckbox =
    List.map
        (\checkbox ->
            li []
                [ input
                    [ type_ "checkbox"
                    , checked checkbox.checked
                    , placeholder checkbox.tag
                    , onClick checkbox.onClick
                    , id checkbox.tag
                    ]
                    []
                , text checkbox.tag
                ]
        )


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleEmail ->
            { model | emailNotifications = not model.emailNotifications }

        ToggleLocation ->
            { model | useLocation = not model.useLocation }

        ToggleAutoplay ->
            { model | videoAutoplay = toggleAutoplay model }

        ToggleAutoplayCtrls ctrl ->
            { model | videoAutoplay = toggleAutoplayCtrls model ctrl }


view : Model -> Html Msg
view model =
    ul []
        (tellUserOptions model)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
