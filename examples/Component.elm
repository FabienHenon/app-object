module Component exposing (Model, Msg, init, subscriptions, update, view)

import AppData exposing (Data, MyAppObject)
import AppObject
import Html exposing (..)
import Html.Events exposing (..)
import Task


type Msg
    = OnClick Bool


type alias Model =
    ()


init : Data -> MyAppObject Model Msg
init data =
    AppObject.init data ()


update : Msg -> Data -> Model -> MyAppObject Model Msg
update msg data model =
    case msg of
        OnClick isModalOpen ->
            AppObject.init data model
                |> AppObject.batchAppCmd (Task.perform AppData.ToggleModal (Task.succeed (not isModalOpen)))


view : Data -> Model -> Html Msg
view data model =
    div []
        [ button [ onClick (OnClick (AppData.isModalOpen data)) ]
            [ if AppData.isModalOpen data then
                text "Close Modal"

              else
                text "Open Modal"
            ]
        ]


subscriptions : Data -> Model -> Sub Msg
subscriptions data model =
    Sub.none
