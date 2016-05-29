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
        SetError _ ->
            ( model, Cmd.none, Nothing )

        SetResponse ( response, prismic ) ->
            case List.head response.results of
                Just result ->
                    fetchRelatedPosts prismic
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


fetchRelatedPosts : P.Cache -> Model -> ( Model, Cmd Msg, Maybe P.Cache )
fetchRelatedPosts prismic model =
    case model.doc of
        Just blogPost ->
            let
                relatedIds =
                    List.filterMap
                        (\related ->
                            case related of
                                P.DocumentLink doc _ ->
                                    Just doc.id

                                _ ->
                                    Nothing
                        )
                        blogPost.relatedPosts
            in
                ( model
                , prismic
                    |> P.fetchApi
                    |> P.form "everything"
                    |> P.query (P.any "document.id" relatedIds)
                    |> P.submit Documents.decodeBlogPost
                    |> Task.perform SetError SetRelatedPosts
                , Just prismic
                )

        Nothing ->
            ( model, Cmd.none, Just prismic )
