module Main exposing (main)

import Svg exposing (Svg)
import Svg.Attributes exposing (..)


main : Svg msg
main =
    Svg.svg
        [ width "800"
        , height "600"
        , viewBox "0 0 800 600"
        ]
        [ Svg.rect
            [ x "0"
            , y "0"
            , width "100"
            , height "100"
            ]
            []
        ]
