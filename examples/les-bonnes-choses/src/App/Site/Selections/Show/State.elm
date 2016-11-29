module App.Site.Selections.Show.State exposing (..)

import App.Site.Selections.Show.Types exposing (..)
import App.Documents.Decoders as Documents
import App.Documents.Types as Documents
import App.Types exposing (GlobalMsg(SetPrismic, RenderNotFound))
import Prismic as P
import Task


init : P.Model -> String -> ( Model, Cmd Msg )
init prismic docId =
    ( { products = Ok []
      , selection = Ok Nothing
      }
    , P.api prismic
        |> P.form "everything"
        |> P.query [ P.at "document.id" docId ]
        |> P.submit Documents.decodeSelection
        |> Task.attempt SetSelection
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
                            ++ if List.isEmpty response.results then
                                [ RenderNotFound ]
                               else
                                []
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


fetchProducts : P.Model -> Documents.Selection -> Cmd Msg
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
        P.api prismic
            |> P.form "products"
            |> P.query [ P.any "document.id" productIds ]
            |> P.submit Documents.decodeProduct
            |> Task.attempt SetProducts
