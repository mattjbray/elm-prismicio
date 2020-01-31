module Result.Extra exposing (collect)


collect : List (Result x a) -> Result x (List a)
collect results =
    let
        go rs acc =
            case rs of
                (Ok x) :: rest ->
                    go rest (acc ++ [ x ])

                (Err err) :: _ ->
                    Err err

                [] ->
                    Ok acc
    in
    go results []
