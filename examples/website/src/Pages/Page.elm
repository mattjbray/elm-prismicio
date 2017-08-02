module Pages.Page exposing (..)

import Documents.Menu exposing (Menu)
import Documents.Page exposing (Page)
import Html exposing (Html)
import Html.Attributes as Html
import Pages.Views exposing (viewBodySlice, viewHeader)


view : Menu -> Page -> Html msg
view menu page =
    Html.div []
        [ viewHeader menu
        , Html.div [ Html.class "container" ]
            (List.map viewBodySlice page.body)
        ]
