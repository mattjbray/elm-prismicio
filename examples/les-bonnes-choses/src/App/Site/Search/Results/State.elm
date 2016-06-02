module App.Site.Search.Results.State exposing (..)

import App.Site.Search.Results.Types exposing (..)
import Prismic.Types as P

init : P.Cache -> String -> ( Model, Cmd Msg )
init prismic query =
  ( { results = () }
  , Cmd.none
  )

update : Msg -> Model -> ( Model, Cmd Msg, List a )
update msg model =
  ( model, Cmd.none, [] )
