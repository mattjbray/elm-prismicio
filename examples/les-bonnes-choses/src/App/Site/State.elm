module App.Site.State exposing (..)

import App.Site.Types exposing (..)
import App.Site.Article.State as Article
import App.Site.Home.State as Home
import App.Site.Products.State as Products
import App.Site.Search.State as Search
import App.Site.Selections.State as Selections
import App.Site.Stores.State as Stores
import App.Types exposing (GlobalMsg(SetPrismic))
import Prismic.Types as P


init : P.Cache -> Page -> ( Model, Cmd Msg )
init prismic page =
    let
        model =
            { page = HomeP
            , content = NoContent
            }

        initWith bookmark =
            initArticle prismic page bookmark model
    in
        case page of
            HomeP as page ->
                let
                    ( home, homeCmd ) =
                        Home.init prismic

                    newModel =
                        { model
                            | page = page
                            , content = HomeC home
                        }
                in
                    newModel ! [ Cmd.map HomeMsg homeCmd ]

            AboutP as page ->
                initWith "about"

            JobsP as page ->
                initWith "jobs"

            (ProductsP productsPage) as page ->
                let
                    ( products, productsCmd ) =
                        Products.init prismic productsPage

                    newModel =
                        { model
                            | page = page
                            , content = ProductsC products
                        }
                in
                    newModel ! [ Cmd.map ProductsMsg productsCmd ]

            (SearchP searchPage) as page ->
                let
                    ( search, searchCmd ) =
                        Search.init prismic searchPage

                    newModel =
                        { model
                            | page = page
                            , content = SearchC search
                        }
                in
                    newModel ! [ Cmd.map SearchMsg searchCmd ]

            (SelectionsP selectionsPage) as page ->
                let
                    ( selections, selectionsCmd ) =
                        Selections.init prismic selectionsPage

                    newModel =
                        { model
                            | page = page
                            , content = SelectionsC selections
                        }
                in
                    newModel ! [ Cmd.map SelectionsMsg selectionsCmd ]

            (StoresP storesPage) as page ->
                let
                    ( stores, storesCmd ) =
                        Stores.init prismic storesPage

                    newModel =
                        { model
                            | page = page
                            , content = StoresC stores
                        }
                in
                    newModel ! [ Cmd.map StoresMsg storesCmd ]


initArticle : P.Cache -> Page -> String -> Model -> ( Model, Cmd Msg )
initArticle prismic page bookmarkName model =
    let
        ( article, articleCmd ) =
            Article.init prismic bookmarkName

        newModel =
            { model
                | page = page
                , content = ArticleC article
            }
    in
        newModel ! [ Cmd.map ArticleMsg articleCmd ]


update : Msg -> Model -> ( Model, Cmd Msg, List GlobalMsg )
update msg model =
    case msg of
        HomeMsg homeMsg ->
            case model.content of
                HomeC home ->
                    let
                        ( newHome, homeCmd, globalMsgs ) =
                            Home.update homeMsg home

                        newModel =
                            { model
                                | content = HomeC newHome
                            }
                    in
                        ( newModel
                        , Cmd.map HomeMsg homeCmd
                        , globalMsgs
                        )

                _ ->
                    ( model, Cmd.none, [] )

        ArticleMsg articleMsg ->
            case model.content of
                ArticleC article ->
                    let
                        ( newArticle, articleCmd, globalMsgs ) =
                            Article.update articleMsg article

                        newModel =
                            { model
                                | content = ArticleC newArticle
                            }
                    in
                        ( newModel
                        , Cmd.map ArticleMsg articleCmd
                        , globalMsgs
                        )

                _ ->
                    ( model, Cmd.none, [] )

        ProductsMsg productsMsg ->
            case model.content of
                ProductsC products ->
                    let
                        ( newProducts, productsCmd, globalMsgs ) =
                            Products.update productsMsg products

                        newModel =
                            { model
                                | content = ProductsC newProducts
                            }
                    in
                        ( newModel
                        , Cmd.map ProductsMsg productsCmd
                        , globalMsgs
                        )

                _ ->
                    ( model, Cmd.none, [] )

        SearchMsg searchMsg ->
            case model.content of
                SearchC search ->
                    let
                        ( newSearch, searchCmd, globalMsgs ) =
                            Search.update searchMsg search

                        newModel =
                            { model
                                | content = SearchC newSearch
                            }
                    in
                        ( newModel
                        , Cmd.map SearchMsg searchCmd
                        , globalMsgs
                        )

                _ ->
                    ( model, Cmd.none, [] )

        SelectionsMsg selectionsMsg ->
            case model.content of
                SelectionsC selections ->
                    let
                        ( newSelections, selectionsCmd, globalMsgs ) =
                            Selections.update selectionsMsg selections

                        newModel =
                            { model
                                | content = SelectionsC newSelections
                            }
                    in
                        ( newModel
                        , Cmd.map SelectionsMsg selectionsCmd
                        , globalMsgs
                        )

                _ ->
                    ( model, Cmd.none, [] )

        StoresMsg storesMsg ->
            case model.content of
                StoresC stores ->
                    let
                        ( newStores, storesCmd, globalMsgs ) =
                            Stores.update storesMsg stores

                        newModel =
                            { model
                                | content = StoresC newStores
                            }
                    in
                        ( newModel
                        , Cmd.map StoresMsg storesCmd
                        , globalMsgs
                        )

                _ ->
                    ( model, Cmd.none, [] )
