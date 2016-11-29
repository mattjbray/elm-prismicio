module Task.Extra exposing (..)

import Task exposing (Task)


fromResult : Result a b -> Task a b
fromResult result =
    case result of
        Ok value ->
            Task.succeed value

        Err err ->
            Task.fail err
