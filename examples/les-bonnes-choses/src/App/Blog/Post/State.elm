module App.Blog.Post.State exposing (..)

import App.Blog.Post.Types exposing (..)
import App.Common exposing (performCompat)
import App.Types exposing (GlobalMsg(SetPrismic, RenderNotFound))
import App.Documents.Decoders as Documents
import Prismic as P


init : P.Model -> String -> ( Model, Cmd Msg )
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
        , P.api prismic
            |> P.form "everything"
            |> P.query [ P.at "document.id" docId ]
            |> P.submit Documents.decodeBlogPost
            |> performCompat SetError SetResponse
        )


update : Msg -> Model -> ( Model, Cmd Msg, List GlobalMsg )
update msg model =
    case msg of
        SetError e ->
            ( { model | error = Just e }
            , Cmd.none
            , []
            )

        SetResponse ( response, prismic ) ->
            case List.head response.results of
                Just result ->
                    fetchRelated prismic
                        { model | doc = Just result.data }

                Nothing ->
                    ( model, Cmd.none, [ SetPrismic prismic, RenderNotFound ] )

        SetRelatedPosts ( response, prismic ) ->
            ( { model
                | relatedPosts = List.map .data response.results
              }
            , Cmd.none
            , [ SetPrismic prismic ]
            )

        SetRelatedProducts ( response, prismic ) ->
            ( { model
                | relatedProducts = List.map .data response.results
              }
            , Cmd.none
            , [ SetPrismic prismic ]
            )


fetchRelated : P.Model -> Model -> ( Model, Cmd Msg, List GlobalMsg )
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
                    [ P.api prismic
                        |> P.form "everything"
                        |> P.query [ P.any "document.id" relatedPostIds ]
                        |> P.submit Documents.decodeBlogPost
                        |> performCompat SetError SetRelatedPosts
                    , P.api prismic
                        |> P.form "everything"
                        |> P.query [ P.any "document.id" relatedProductIds ]
                        |> P.submit Documents.decodeProduct
                        |> performCompat SetError SetRelatedProducts
                    ]
                , [ SetPrismic prismic ]
                )

        Nothing ->
            ( model, Cmd.none, [ SetPrismic prismic ] )
