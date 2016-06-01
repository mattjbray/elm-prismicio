module App.Site.Selections.Show.State exposing (..)

import App.Site.Selections.Show.Types exposing (..)
import App.Documents.Decoders as Documents
import App.Documents.Types as Documents
import App.Types exposing (GlobalMsg(SetPrismic))
import Basics.Extra exposing (never)
import Prismic.Types as P
import Prismic as P
import Task


init : P.Cache -> String -> ( Model, Cmd Msg )
init prismic docId =
    ( { products = Ok []
      , selection = Ok Nothing
      }
    , prismic
        |> P.fetchApi
        |> P.form "everything"
        |> P.query [ P.at "document.id" docId ]
        |> P.submit Documents.decodeSelection
        |> Task.toResult
        |> Task.perform never SetSelection
    )


update : Msg -> Model -> ( Model, Cmd Msg, List GlobalMsg )
update msg model =
    case msg of
        SetSelection result ->
            case result of
                Err error ->
                    ( { model
                        | selection = Err error
                      }
                    , Cmd.none
                    , []
                    )

                Ok ( response, prismic ) ->
                    let
                        mSelection =
                            response.results
                                |> List.head
                                |> Maybe.map .data
                    in
                        ( { model
                            | selection = Ok mSelection
                          }
                        , mSelection
                            |> Maybe.map (fetchProducts prismic)
                            |> Maybe.withDefault Cmd.none
                        , [ SetPrismic prismic ]
                        )

        SetProducts result ->
            case result of
                Err error ->
                    ( { model
                        | products = Err error
                      }
                    , Cmd.none
                    , []
                    )

                Ok ( response, prismic ) ->
                    ( { model
                        | products = Ok (List.map .data response.results)
                      }
                    , Cmd.none
                    , [ SetPrismic prismic ]
                    )


fetchProducts : P.Cache -> Documents.Selection -> Cmd Msg
fetchProducts prismic selection =
    let
        productIds =
            selection.products
                |> List.filterMap
                    (\product ->
                        case product of
                            P.DocumentLink doc _ ->
                                Just doc.id

                            _ ->
                                Nothing
                    )
    in
        prismic
            |> P.fetchApi
            |> P.form "products"
            |> P.query [ P.any "document.id" productIds ]
            |> P.submit Documents.decodeProduct
            |> Task.toResult
            |> Task.perform never SetProducts
