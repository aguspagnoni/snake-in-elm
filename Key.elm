module Key exposing (..)


type Key
    = Space
    | ArrowLeft
    | ArrowRight
    | ArrowDown
    | ArrowUp
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        32 ->
            Space

        37 ->
            ArrowLeft

        39 ->
            ArrowRight

        38 ->
            ArrowUp

        40 ->
            ArrowDown

        _ ->
            Unknown
