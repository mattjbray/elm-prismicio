module Pages.Page exposing (..)

import Documents.Menu exposing (Menu)
import Documents.Page exposing (Page)
import Html exposing (Html)
import Html.Attributes as Html
import Pages.Views exposing (viewBodySlice, viewHeader)
import Prismic.Document.Field as Prismic


view : Prismic.LinkResolver msg -> Menu -> Page -> Html msg
view linkResolver menu page =
    Html.div []
        [ viewHeader linkResolver menu
        , Html.div [ Html.class "container" ]
            (List.map viewBodySlice page.body)
        ]
