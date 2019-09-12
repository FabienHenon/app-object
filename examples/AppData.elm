module AppData exposing (AppMsg(..), Data, MyAppObject, init, isModalOpen, setModalOpen)

import AppObject


type alias MyAppObject model msg =
    AppObject.AppObject Data AppMsg model msg


type alias Data =
    { isModalOpen : Bool
    }


type AppMsg
    = ToggleModal Bool


init : Data
init =
    { isModalOpen = False
    }


setModalOpen : Bool -> Data -> Data
setModalOpen isModalOpen_ data =
    { data | isModalOpen = isModalOpen_ }


isModalOpen : Data -> Bool
isModalOpen data =
    data.isModalOpen
