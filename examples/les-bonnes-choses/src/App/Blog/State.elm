module App.Blog.State exposing (..)

import App.Blog.Types exposing (..)
import App.Blog.Index.State as Index
import App.Blog.Post.State as Post
import Prismic.Types as P


init : P.Cache -> Page -> ( Model, Cmd Msg )
init prismic page =
    case page of
        IndexP category ->
            let
                ( index, indexCmd ) =
                    Index.init prismic category
            in
                ( { page = page
                  , content = IndexC index
                  }
                , Cmd.map IndexMsg indexCmd
                )

        PostP docId ->
            let
                ( post, postCmd ) =
                    Post.init prismic docId
            in
                ( { page = page
                  , content = PostC post
                  }
                , Cmd.map PostMsg postCmd
                )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe P.Cache )
update msg model =
    case msg of
        IndexMsg indexMsg ->
            case model.content of
                IndexC index ->
                    let
                        ( newIndex, indexCmd, mNewPrismic ) =
                            Index.update indexMsg index
                    in
                        ( { model | content = IndexC newIndex }
                        , Cmd.map IndexMsg indexCmd
                        , mNewPrismic
                        )

                _ ->
                    ( model, Cmd.none, Nothing )

        PostMsg postMsg ->
            case model.content of
                PostC post ->
                    let
                        ( newPost, postCmd, mNewPrismic ) =
                            Post.update postMsg post
                    in
                        ( { model | content = PostC newPost }
                        , Cmd.map PostMsg postCmd
                        , mNewPrismic
                        )

                _ ->
                    ( model, Cmd.none, Nothing )
