module App.Site.Stores.Show.State exposing (..)

import App.Site.Stores.Show.Types exposing (..)
import App.Documents.Decoders as Documents
import App.Types exposing (GlobalMsg(SetPrismic))
import Basics.Extra exposing (never)
import Prismic.Types as P
import Prismic as P
import Task


init : P.Cache -> String -> ( Model, Cmd Msg )
init prismic docId =
    ( { store =
          Ok Nothing
      }
    , prismic
        |> P.fetchApi
        |> P.form "everything"
        |> P.query [ P.at "document.id" docId ]
        |> P.submit Documents.decodeStore
        |> Task.toResult
        |> Task.perform never SetStore
    )


update : Msg -> Model -> ( Model, Cmd Msg, List GlobalMsg )
update msg model =
    case msg of
        SetStore result ->
            case result of
                Err error ->
                    ( { model
                        | store = Err error
                      }
                    , Cmd.none
                    , []
                    )

                Ok ( response, prismic ) ->
                    let
                        mStore =
                            response.results
                                |> List.head
                                |> Maybe.map .data
                    in
                        ( { model
                            | store = Ok mStore
                          }
                        , Cmd.none
                        , [ SetPrismic prismic ]
                        )
