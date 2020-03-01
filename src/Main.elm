module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, div, form, h3, input, label, li, text, ul)
import Html.Attributes exposing (checked, class, classList, for, id, placeholder, type_)
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
    { isChecked : Bool
    , tag : String
    , fieldId : Field
    , handleClick : Msg
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


doId : String -> String
doId id =
    String.replace " " "-" id
        |> String.toLower


getVideoAutoplaySettings : Autoplay -> AutoplaySettings
getVideoAutoplaySettings autoplay =
    case autoplay of
        On ctrls ->
            ctrls

        Off ctrls ->
            ctrls


getVideoAutoplaySettingCtrlStatus : AutoplayCtrls -> AutoplaySettings -> Bool
getVideoAutoplaySettingCtrlStatus ctrl autoplaySettings =
    case ctrl of
        AudioCtrl ->
            autoplaySettings.audio

        WithOutWifiCtrl ->
            autoplaySettings.withoutwifi


isVideoAutoplayOn : Model -> Bool
isVideoAutoplayOn { videoAutoplay } =
    case videoAutoplay of
        On _ ->
            True

        Off _ ->
            False


isAutoplaySettingCtrlChecked : Model -> AutoplayCtrls -> Bool
isAutoplaySettingCtrlChecked { videoAutoplay } ctrl =
    getVideoAutoplaySettings videoAutoplay
        |> getVideoAutoplaySettingCtrlStatus ctrl


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
                    { audio = not (isAutoplaySettingCtrlChecked model AudioCtrl)
                    , withoutwifi = isAutoplaySettingCtrlChecked model WithOutWifiCtrl
                    }

                WithOutWifiCtrl ->
                    { audio = isAutoplaySettingCtrlChecked model AudioCtrl
                    , withoutwifi = not (isAutoplaySettingCtrlChecked model WithOutWifiCtrl)
                    }
    in
    case model.videoAutoplay of
        Off _ ->
            Off ctrlsSetting

        On _ ->
            On ctrlsSetting


getCheckboxList : Model -> List Checkbox
getCheckboxList model =
    [ { isChecked = model.emailNotifications
      , tag = "Email Notifications"
      , fieldId = Email
      , handleClick = ToggleEmail
      }
    , { isChecked = isVideoAutoplayOn model
      , tag = "Video autoplay"
      , fieldId = Video
      , handleClick = ToggleAutoplay
      }
    , { isChecked =
            isAutoplaySettingCtrlChecked model AudioCtrl
      , tag = "Audio"
      , fieldId = Audio
      , handleClick = ToggleAutoplayCtrls AudioCtrl
      }
    , { isChecked =
            isAutoplaySettingCtrlChecked model WithOutWifiCtrl
      , tag = "WithOutWifi"
      , fieldId = Wifi
      , handleClick = ToggleAutoplayCtrls WithOutWifiCtrl
      }
    , { isChecked = model.useLocation
      , tag = "Use location"
      , fieldId = Location
      , handleClick = ToggleLocation
      }
    ]


audioSettingsCtlsFields : List Field
audioSettingsCtlsFields =
    [ Audio
    , Wifi
    ]


isAudioSettingsCtrlsFieldMember : Field -> Bool
isAudioSettingsCtrlsFieldMember field =
    List.member field audioSettingsCtlsFields


dropAutoPlaySettings : List Checkbox -> List Checkbox
dropAutoPlaySettings =
    List.filter
        (\{ fieldId } ->
            not
                (isAudioSettingsCtrlsFieldMember fieldId)
        )


doStyleForFields : Field -> Attribute msg
doStyleForFields field =
    classList
        [ ( "ph4"
          , isAudioSettingsCtrlsFieldMember field
          )
        , ( "ph3"
          , not
                (isAudioSettingsCtrlsFieldMember field)
          )
        , ( "tl pv2 bb b--light-silver pointer", True )
        ]


doHtmlCheckBoxList : List Checkbox -> List (Html Msg)
doHtmlCheckBoxList =
    List.map
        (\{ isChecked, tag, handleClick, fieldId } ->
            li
                [ doStyleForFields fieldId
                ]
                [ input
                    [ type_ "checkbox"
                    , checked isChecked
                    , placeholder tag
                    , onClick handleClick
                    , id (doId tag)
                    ]
                    []
                , label
                    [ for (doId tag)
                    , class "ph2"
                    ]
                    [ text tag
                    ]
                ]
        )


tellUserOptions : Model -> List (Html Msg)
tellUserOptions model =
    if isVideoAutoplayOn model then
        getCheckboxList model
            |> doHtmlCheckBoxList

    else
        getCheckboxList model
            |> dropAutoPlaySettings
            |> doHtmlCheckBoxList


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
    div []
        [ h3 [ class "f3 f3-m " ] [ text "Hello, select your options please" ]
        , form []
            [ ul [ class "list pl0 ml0 center mw5 ba b--light-silver br3" ]
                (tellUserOptions model)
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
