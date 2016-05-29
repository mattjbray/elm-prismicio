module App.Blog.Post.State exposing (..)

import App.Blog.Post.Types exposing (..)
import App.Documents.Decoders as Documents
import Prismic.Types as P exposing (Url(Url))
import Prismic.State as P
import Prismic as P
import Task
import String


init : String -> ( Model, Cmd Msg )
init docId =
    let
        model =
            { doc =
                Nothing
            , relatedPosts =
                []
            , prismic =
                P.initCache (Url "https://lesbonneschoses.prismic.io/api")
            }
    in
        ( model
        , model.prismic
            |> P.fetchApi
            |> P.form "everything"
            |> P.query ("[[:d = at(document.id, \"" ++ docId ++ "\")]]")
            |> P.submit Documents.decodeBlogPost
            |> Task.perform SetError SetResponse
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetError _ ->
            model ! []

        SetResponse ( response, cache ) ->
            let
                newModel =
                    { model | prismic = cache }
            in
                case List.head response.results of
                    Just result ->
                        fetchRelatedPosts
                            { newModel
                                | doc = Just result.data
                            }

                    Nothing ->
                        newModel ! []

        SetRelatedPosts ( response, cache ) ->
            { model
                | prismic = cache
                , relatedPosts = List.map .data response.results
            }
                ! []


fetchRelatedPosts : Model -> ( Model, Cmd Msg )
fetchRelatedPosts model =
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

                query =
                    "[[:d = any(document.id, ["
                        ++ String.join ", " (List.map (\id -> "\"" ++ id ++ "\"") relatedIds)
                        ++ "])]]"
            in
                ( model
                , model.prismic
                    |> P.fetchApi
                    |> P.form "everything"
                    |> P.query query
                    |> P.submit Documents.decodeBlogPost
                    |> Task.perform SetError SetRelatedPosts
                )

        Nothing ->
            model ! []
