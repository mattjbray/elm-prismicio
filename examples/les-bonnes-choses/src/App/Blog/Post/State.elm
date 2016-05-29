module App.Blog.Post.State exposing (..)

import App.Blog.Post.Types exposing (..)
import App.Documents.Decoders as Documents
import Prismic.Types as P exposing (Url(Url))
import Prismic.State as P
import Prismic as P
import Task


init : String -> ( Model, Cmd Msg )
init docId =
    let
        model =
            { doc =
                Nothing
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
                        { newModel
                            | doc = Just result.data
                        }
                            ! []

                    Nothing ->
                        newModel ! []
