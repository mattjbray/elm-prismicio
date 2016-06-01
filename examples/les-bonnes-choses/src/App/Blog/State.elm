module App.Blog.State exposing (..)

import App.Types exposing (GlobalMsg(SetPrismic))
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

        PostP docId _ ->
            let
                ( post, postCmd ) =
                    Post.init prismic docId
            in
                ( { page = page
                  , content = PostC post
                  }
                , Cmd.map PostMsg postCmd
                )


update : Msg -> Model -> ( Model, Cmd Msg, List GlobalMsg )
update msg model =
    case msg of
        IndexMsg indexMsg ->
            case model.content of
                IndexC index ->
                    let
                        ( newIndex, indexCmd, globalMsgs ) =
                            Index.update indexMsg index
                    in
                        ( { model | content = IndexC newIndex }
                        , Cmd.map IndexMsg indexCmd
                        , globalMsgs
                        )

                _ ->
                    ( model, Cmd.none, [] )

        PostMsg postMsg ->
            case model.content of
                PostC post ->
                    let
                        ( newPost, postCmd, globalMsgs ) =
                            Post.update postMsg post
                    in
                        ( { model | content = PostC newPost }
                        , Cmd.map PostMsg postCmd
                        , globalMsgs
                        )

                _ ->
                    ( model, Cmd.none, [] )
