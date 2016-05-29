module App.Blog.State exposing (..)

import App.Blog.Navigation exposing (toUrl)
import App.Blog.Types exposing (..)
import App.Blog.Index.State as Index
import App.Blog.Post.State as Post
import Navigation


init : Page -> (Model, Cmd Msg)
init page =
    case page of
        IndexP ->
            let
                ( index, indexCmd ) =
                    Index.init
            in
                ( { page = page
                  , content = IndexC index
                  }
                , Cmd.map IndexMsg indexCmd
                )
        PostP docId ->
            let
                ( post, postCmd ) =
                    Post.init docId
            in
                ( { page = page
                  , content = PostC post
                  }
                , Cmd.map PostMsg postCmd
                )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo page ->
            model
                ! if model.page == page then
                    []
                  else
                    [ Navigation.newUrl (toUrl page) ]

        IndexMsg indexMsg ->
            case model.content of
                IndexC index ->
                    let
                        ( newIndex, indexCmd ) =
                            Index.update indexMsg index
                    in
                        ( { model | content = IndexC newIndex }
                        , Cmd.map IndexMsg indexCmd
                        )

                _ ->
                    model ! []

        PostMsg postMsg ->
            case model.content of
                PostC post ->
                    let
                        ( newPost, postCmd ) =
                            Post.update postMsg post
                    in
                        ( { model | content = PostC newPost }
                        , Cmd.map PostMsg postCmd
                        )

                _ ->
                    model ! []
