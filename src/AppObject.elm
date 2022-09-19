module AppObject exposing
    ( AppObject
    , init, batchCmd, batchAppCmd, batchCmdWithModel, batchAppCmdWithModel
    , map, mapAppData, mapAppMsg, mapModel, mapMsg
    , merge, foldl, andThen
    , run
    )

{-| The `AppObject` is an object that encapsulates the `Model` and `Cmd Msg` of a component (returned in side effect functions like `init` or `update`) **plus** and `appData` object and a `Cmd appMsg`.
These new elements are a model and commands related to the entire application.

Let's say for instance that you have a component that does something (whatever) and that needs to open a modal, or set the connected user, or set the user language of the application, ...
All of these states/effects are related to the application, not to the component.
Indeed, the component itself can't open a modal because it's the application's responsibility.
Also, the component could save the connected user to its own state, but it's a data the entire application would like to know about and with the classic architecture of elm apps it's not
possible for a component to tell the app that it should update its state.

To make it simpler: a component can't change the state of its parent component and can't send commands with its parent's messages.

This is what `AppObject` is for, it allows you to:

  - define a model that any component in the application can read and update
  - send commands that the root component can handle


# Definitions

@docs AppObject


# Init

@docs init, batchCmd, batchAppCmd, batchCmdWithModel, batchAppCmdWithModel


# Mappers

@docs map, mapAppData, mapAppMsg, mapModel, mapMsg


# Advanced usage

@docs merge, foldl, andThen


# Usage

@docs run

-}


{-| `AppObject` is the object you should return from your side effect functions (`init`, `update`) in your components.

  - `appData` refers to the model you want to be global to your application. This model is usualy stored in your root state
  - `appMsg` refers to the `msg` type you want to be global to your application. This `msg` is usually handle in your root `update` function
  - `model` refers to the model of your current component
  - `msg` refers to the `msg` type of your current component

I suggest your create a module that defines your `appData` model and your `appMsg` message type, and you define a `type alias` of your own `AppObject` with
the `appData` and `appMsg` set :

    type alias MyAppObject model msg =
        AppObject.AppObject Data AppMsg model msg

    type alias Data =
        { isModalOpen : Bool
        }

    type AppMsg
        = ToggleModal Bool

-}
type AppObject appData appMsg model msg
    = AppObject (AppObject_ appData appMsg model msg)


type alias AppObject_ appData appMsg model msg =
    { model : model
    , appData : appData
    , cmd : Cmd msg
    , appCmd : Cmd appMsg
    }


{-| Creates a new `AppObject` from the `appData` and your component's model
-}
init : appData -> model -> AppObject appData appMsg model msg
init appData model =
    AppObject
        { model = model
        , appData = appData
        , cmd = Cmd.none
        , appCmd = Cmd.none
        }


{-| Adds a new global command to this `AppObject`
-}
batchAppCmd : Cmd appMsg -> AppObject appData appMsg model msg -> AppObject appData appMsg model msg
batchAppCmd appCmd (AppObject ao) =
    if appCmd == Cmd.none then
        AppObject ao

    else
        AppObject { ao | appCmd = Cmd.batch [ ao.appCmd, appCmd ] }


{-| Adds a new component's command to this `AppObject`
-}
batchCmd : Cmd msg -> AppObject appData appMsg model msg -> AppObject appData appMsg model msg
batchCmd cmd (AppObject ao) =
    if cmd == Cmd.none then
        AppObject ao

    else
        AppObject { ao | cmd = Cmd.batch [ ao.cmd, cmd ] }


{-| Adds a new global command to this `AppObject`. The model is passed as parameter
-}
batchAppCmdWithModel : (model -> ( model, Cmd appMsg )) -> AppObject appData appMsg model msg -> AppObject appData appMsg model msg
batchAppCmdWithModel appCmd (AppObject ao) =
    let
        ( newModel, finalCmd ) =
            appCmd ao.model
    in
    if finalCmd == Cmd.none then
        AppObject { ao | model = newModel }

    else
        AppObject { ao | appCmd = Cmd.batch [ ao.appCmd, finalCmd ], model = newModel }


{-| Adds a new component's command to this `AppObject`? The model is passed as parameter
-}
batchCmdWithModel : (model -> ( model, Cmd msg )) -> AppObject appData appMsg model msg -> AppObject appData appMsg model msg
batchCmdWithModel cmd (AppObject ao) =
    let
        ( newModel, finalCmd ) =
            cmd ao.model
    in
    if finalCmd == Cmd.none then
        AppObject { ao | model = newModel }

    else
        AppObject { ao | cmd = Cmd.batch [ ao.cmd, finalCmd ], model = newModel }


{-| Maps your `AppObject` to change the types of `appData`, `appMsg`, `model` and `msg`
-}
map : (modelA -> modelB) -> (appDataA -> appDataB) -> (msgA -> msgB) -> (appMsgA -> appMsgB) -> AppObject appDataA appMsgA modelA msgA -> AppObject appDataB appMsgB modelB msgB
map mapModel_ mapAppData_ mapMsg_ mapAppMsg_ (AppObject ao) =
    AppObject
        { model = mapModel_ ao.model
        , appData = mapAppData_ ao.appData
        , cmd = Cmd.map mapMsg_ ao.cmd
        , appCmd = Cmd.map mapAppMsg_ ao.appCmd
        }


{-| Maps only your `model`
-}
mapModel : (a -> model) -> AppObject appData appMsg a msg -> AppObject appData appMsg model msg
mapModel mapper (AppObject ao) =
    AppObject
        { model = mapper ao.model
        , appData = ao.appData
        , cmd = ao.cmd
        , appCmd = ao.appCmd
        }


{-| Maps only your `appData`
-}
mapAppData : (a -> appData) -> AppObject a appMsg model msg -> AppObject appData appMsg model msg
mapAppData mapper (AppObject ao) =
    AppObject
        { model = ao.model
        , appData = mapper ao.appData
        , cmd = ao.cmd
        , appCmd = ao.appCmd
        }


{-| Maps only your `msg`
-}
mapMsg : (a -> msg) -> AppObject appData appMsg model a -> AppObject appData appMsg model msg
mapMsg mapper (AppObject ao) =
    AppObject
        { model = ao.model
        , appData = ao.appData
        , cmd = Cmd.map mapper ao.cmd
        , appCmd = ao.appCmd
        }


{-| Maps only your `appMsg`
-}
mapAppMsg : (a -> appMsg) -> AppObject appData a model msg -> AppObject appData appMsg model msg
mapAppMsg mapper (AppObject ao) =
    AppObject
        { model = ao.model
        , appData = ao.appData
        , cmd = ao.cmd
        , appCmd = Cmd.map mapper ao.appCmd
        }


{-| Merges 2 `AppObject`s together. You provide the functions used to:

  - merge the `model`
  - merge the `appData`
  - map the `msg`
  - map the `appMsg`

-}
merge : (modelB -> modelA -> modelC) -> (appDataB -> appDataA -> appDataC) -> (msgA -> msgC) -> (msgB -> msgC) -> (appMsgA -> appMsgC) -> (appMsgB -> appMsgC) -> AppObject appDataA appMsgA modelA msgA -> AppObject appDataB appMsgB modelB msgB -> AppObject appDataC appMsgC modelC msgC
merge mergerModel mergerAppData mergerMsgA mergerMsgB mergerAppMsgA mergerAppMsgB (AppObject pd1) (AppObject pd2) =
    AppObject
        { model = mergerModel pd2.model pd1.model
        , appData = mergerAppData pd2.appData pd1.appData
        , cmd =
            if pd2.cmd == Cmd.none && pd1.cmd == Cmd.none then
                Cmd.none

            else if pd2.cmd == Cmd.none then
                Cmd.map mergerMsgA pd1.cmd

            else if pd1.cmd == Cmd.none then
                Cmd.map mergerMsgB pd2.cmd

            else
                Cmd.batch [ Cmd.map mergerMsgB pd2.cmd, Cmd.map mergerMsgA pd1.cmd ]
        , appCmd =
            if pd2.appCmd == Cmd.none && pd1.appCmd == Cmd.none then
                Cmd.none

            else if pd2.appCmd == Cmd.none then
                Cmd.map mergerAppMsgA pd1.appCmd

            else if pd1.appCmd == Cmd.none then
                Cmd.map mergerAppMsgB pd2.appCmd

            else
                Cmd.batch [ Cmd.map mergerAppMsgA pd1.appCmd, Cmd.map mergerAppMsgB pd2.appCmd ]
        }


{-| Allows you to create a new `AppObject` from an existing one, using the `appData` and the `model`. The new `Cmd appMsg` and `Cmd msg` will be batched to the
existing ones
-}
andThen : (appDataA -> modelA -> AppObject appDataB appMsg modelB msg) -> AppObject appDataA appMsg modelA msg -> AppObject appDataB appMsg modelB msg
andThen func (AppObject ao) =
    func ao.appData ao.model
        |> batchCmd ao.cmd
        |> batchAppCmd ao.appCmd


{-| Reducer that allows you to update or create an `AppObject` for each element of a list
-}
foldl : (a -> AppObject appData appMsg model msg -> AppObject appData appMsg model msg) -> List a -> AppObject appData appMsg model msg -> AppObject appData appMsg model msg
foldl func items appObject =
    List.foldl func appObject items


{-| Executes your `AppObject`. That means that you will create a `( model, Cmd msg )` tuple from an `AppObject`, this allows you to run your side effects and model updates
in your root `update` and `init` functions
-}
run : (appData -> model -> model) -> (appMsg -> msg) -> AppObject appData appMsg model msg -> ( model, Cmd msg )
run appDataMapper appMsgMapper (AppObject ao) =
    ( appDataMapper ao.appData ao.model
    , Cmd.batch [ ao.cmd, Cmd.map appMsgMapper ao.appCmd ]
    )
