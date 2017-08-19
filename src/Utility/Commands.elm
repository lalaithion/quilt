module Commands exposing (..)


doNothing : a -> ( a, Cmd msg )
doNothing a =
    ( a, Cmd.none )
