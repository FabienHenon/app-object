module Main exposing (main)

import AppData exposing (AppMsg, Data, MyAppObject)
import AppObject
import Browser
import Component
import Html exposing (..)


type Msg
    = ComponentMsg Component.Msg
    | AppMsgHandler AppMsg


type alias Model =
    { component : Component.Model
    , data : Data
    }


init : () -> ( Model, Cmd Msg )
init flags =
    let
        data =
            AppData.init

        appObject =
            Component.init data
                |> AppObject.andThen (\data_ component_ -> AppObject.init data_ { component = component_, data = data_ })
                |> AppObject.mapMsg ComponentMsg
    in
    appObject
        |> AppObject.run mergeAppData AppMsgHandler


mergeAppData : Data -> Model -> Model
mergeAppData data model =
    { model | data = data }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ComponentMsg msg_ ->
            AppObject.init model.data model.component
                |> AppObject.andThen (Component.update msg_)
                |> AppObject.mapMsg ComponentMsg
                |> AppObject.mapModel (\component -> { model | component = component })
                |> AppObject.run mergeAppData AppMsgHandler

        AppMsgHandler (AppData.ToggleModal isModalOpen) ->
            AppObject.init (AppData.setModalOpen isModalOpen model.data) model
                |> AppObject.run mergeAppData AppMsgHandler


view : Model -> Html Msg
view model =
    div []
        [ Component.view model.data model.component |> Html.map ComponentMsg
        , div []
            [ if AppData.isModalOpen model.data then
                text "The modal is opened (ok it's not really a modal...)"

              else
                text "The modal is closed"
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ComponentMsg <| Component.subscriptions model.data model.component


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
