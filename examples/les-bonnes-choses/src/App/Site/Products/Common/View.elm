module App.Site.Products.Common.View exposing (..)

import String


toCurrency : Float -> String
toCurrency amount =
    let
        amountStr =
            toString amount

        amountStr' =
            if String.contains "." amountStr then
                amountStr
            else
                (amountStr ++ ".")

        parts =
            String.split "." amountStr'

        build strs =
            case strs of
                [ str ] ->
                    String.padRight 2 '0' str :: build []

                str :: rest ->
                    str :: build rest

                [] ->
                    []
    in
        "$" ++ String.join "." (build parts)
