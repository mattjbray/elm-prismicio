module Main exposing (main)

import App.State exposing (init, update, urlUpdate)
import App.Navigation exposing (hashParser)
import App.View exposing (view)
import Navigation


main : Program Never
main =
    Navigation.program (Navigation.makeParser hashParser)
        { init = init
        , update = update
        , view = view
        , urlUpdate = urlUpdate
        , subscriptions = always Sub.none
        }
