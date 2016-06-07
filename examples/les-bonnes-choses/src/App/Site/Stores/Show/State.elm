module App.Site.Stores.Show.State exposing (..)

import App.Site.Stores.Show.Types exposing (..)
import App.Documents.Decoders as Documents
import App.Types exposing (GlobalMsg(SetPrismic, RenderNotFound))
import App.Ports exposing (googleMap)
import Basics.Extra exposing (never)
import Prismic as P
import Task
import String


init : P.Model -> String -> ( Model, Cmd Msg )
init prismic docId =
    ( { store =
            Ok Nothing
      }
    , P.api prismic
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
                    case List.head response.results of
                        Nothing ->
                            ( { model
                                | store = Ok Nothing
                              }
                            , Cmd.none
                            , [ SetPrismic prismic
                              , RenderNotFound
                              ]
                            )

                        Just result ->
                            let
                                store =
                                    result.data

                                address =
                                    String.join " "
                                        [ store.address
                                        , store.city
                                        , store.zipcode
                                        , store.country
                                        ]
                            in
                                ( { model
                                    | store = Ok (Just store)
                                  }
                                , googleMap ("map-canvas", address)
                                , [ SetPrismic prismic ]
                                )
