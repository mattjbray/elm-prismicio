module Main exposing (main)

import App.State exposing (init, update)
import App.View exposing (view)
import Html.App as Html


main : Program Never
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
