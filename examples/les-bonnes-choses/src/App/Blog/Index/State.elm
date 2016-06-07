module App.Blog.Index.State exposing (..)

import App.Blog.Index.Types exposing (..)
import App.Types exposing (GlobalMsg(SetPrismic))
import App.Documents.Decoders as Documents
import Prismic as P exposing (Url(Url))
import Task


init : P.Model -> Maybe String -> ( Model, Cmd Msg )
init prismic mCategory =
    let
        model =
            { docs =
                Nothing
            }
    in
        ( model
        , P.api prismic
            |> P.form "blog"
            |> (mCategory
                    |> Maybe.map
                        (\category ->
                            P.query [ P.at "my.blog-post.category" category ]
                        )
                    |> Maybe.withDefault P.none
               )
            |> P.submit Documents.decodeBlogPost
            |> Task.perform SetError SetResponse
        )


update : Msg -> Model -> ( Model, Cmd Msg, List GlobalMsg )
update msg model =
    case msg of
        SetError _ ->
            ( model, Cmd.none, [] )

        SetResponse ( response, prismic ) ->
            ( { model
                | docs = Just (List.map .data response.results)
              }
            , Cmd.none
            , [ SetPrismic prismic ]
            )
