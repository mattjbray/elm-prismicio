module App.Blog.Post.State exposing (..)

import App.Blog.Post.Types exposing (..)
import App.Documents.Decoders as Documents
import Prismic.Types as P
import Prismic as P
import Task


init : P.Cache -> String -> ( Model, Cmd Msg )
init prismic docId =
    let
        model =
            { doc =
                Nothing
            , relatedPosts =
                []
            , relatedProducts =
                []
            , error =
                Nothing
            }
    in
        ( model
        , prismic
            |> P.fetchApi
            |> P.form "everything"
            |> P.query (P.at "document.id" docId)
            |> P.submit Documents.decodeBlogPost
            |> Task.perform SetError SetResponse
        )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe P.Cache )
update msg model =
    case msg of
        SetError e ->
            ( { model | error = Just e }
            , Cmd.none
            , Nothing
            )

        SetResponse ( response, prismic ) ->
            case List.head response.results of
                Just result ->
                    fetchRelated prismic
                        { model | doc = Just result.data }

                Nothing ->
                    ( model, Cmd.none, Just prismic )

        SetRelatedPosts ( response, prismic ) ->
            ( { model
                | relatedPosts = List.map .data response.results
              }
            , Cmd.none
            , Just prismic
            )

        SetRelatedProducts ( response, prismic ) ->
            ( { model
                | relatedProducts = List.map .data response.results
              }
            , Cmd.none
            , Just prismic
            )


fetchRelated : P.Cache -> Model -> ( Model, Cmd Msg, Maybe P.Cache )
fetchRelated prismic model =
    case model.doc of
        Just blogPost ->
            let
                getDocIds links =
                    List.filterMap
                        (\related ->
                            case related of
                                P.DocumentLink doc _ ->
                                    Just doc.id

                                _ ->
                                    Nothing
                        )
                        links

                relatedPostIds =
                    getDocIds blogPost.relatedPosts

                relatedProductIds =
                    getDocIds blogPost.relatedProducts
            in
                ( model
                , Cmd.batch
                     [ prismic
                        |> P.fetchApi
                        |> P.form "everything"
                        |> P.query (P.any "document.id" relatedPostIds)
                        |> P.submit Documents.decodeBlogPost
                        |> Task.perform SetError SetRelatedPosts
                     , prismic
                        |> P.fetchApi
                        |> P.form "everything"
                        |> P.query (P.any "document.id" relatedProductIds)
                        |> P.submit Documents.decodeProduct
                        |> Task.perform SetError SetRelatedProducts
                     ]
                , Just prismic
                )

        Nothing ->
            ( model, Cmd.none, Just prismic )
