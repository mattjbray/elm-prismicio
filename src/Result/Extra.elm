module Result.Extra exposing (collect)

collect : List (Result x a) -> Result x (List a)
collect results =
    let
        go rs acc =
            case rs of
                (Ok x) :: rest ->
                    go rest (acc ++ [ x ])

                (Err err) :: rest ->
                    Err err

                [] ->
                    Ok acc
    in
        go results []
