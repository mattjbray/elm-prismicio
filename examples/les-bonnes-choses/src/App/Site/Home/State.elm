module App.Site.Home.State exposing (..)

import App.Site.Home.Types exposing (..)
import Prismic.Types as P


init : P.Cache -> ( Model, Cmd Msg )
init prismic =
    { doc = Nothing } ! []


update : Msg -> Model -> ( Model, Cmd Msg, Maybe P.Cache )
update msg model =
    ( model, Cmd.none, Nothing )
