module Pages.Page exposing (..)

import Documents.Menu exposing (Menu)
import Documents.Page exposing (Page)
import Html exposing (Html)
import Html.Attributes as Html
import Pages.Views exposing (viewBodySlice, viewHeader)
import Prismic.Document as Prismic


view : Menu -> Page -> Html Prismic.DocumentReference
view menu page =
    Html.div []
        [ viewHeader menu
        , Html.div [ Html.class "container" ]
            (List.map viewBodySlice page.body)
        ]
