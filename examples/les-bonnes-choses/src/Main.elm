module Main exposing (main)

import App.State exposing (init, update)
import App.View exposing (view)
import App.Types exposing (..)
import Navigation


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
