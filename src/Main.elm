module Main exposing (main)

import Dict exposing (Dict)
import List.Extra as List
import Random
import Svg exposing (Svg)
import Svg.Attributes exposing (..)


puzzle =
    { draftMode = True
    , piecesX = 12
    , piecesY = 6
    , pixelsPerCell = 50
    , seed = Random.initialSeed 666
    , gridPerturb = 6
    }


params =
    { width = puzzle.piecesX * puzzle.pixelsPerCell
    , height = puzzle.piecesY * puzzle.pixelsPerCell
    }


type alias Point =
    { x : Int
    , y : Int
    }


type alias Edge =
    { start : Point
    , end : Point
    }


type alias Curve3 =
    { start : Point
    , startControl : Point
    , middle : Point
    , middleControl : Point
    , endControl : Point
    , end : Point
    }


main : Svg msg
main =
    let
        grid =
            rectangularGrid puzzle.piecesX puzzle.piecesY
                |> perturbGrid

        markers =
            Dict.values grid
                |> List.map drawMarker

        edges =
            calcEdges grid
    in
    canvas params.width
        params.height
        [ Svg.g [] markers
        , Svg.g [] <| List.map drawEdge edges
        , drawCurve3 basicTongue
        ]


rectangularGrid : Int -> Int -> Dict ( Int, Int ) Point
rectangularGrid nx ny =
    let
        indicesX =
            List.range 0 nx

        indicesY =
            List.range 0 ny

        indices =
            List.lift2 Tuple.pair indicesX indicesY
    in
    indices
        |> List.map
            (\( ix, iy ) ->
                ( ( ix, iy )
                , { x = ix * puzzle.pixelsPerCell
                  , y = iy * puzzle.pixelsPerCell
                  }
                )
            )
        |> Dict.fromList


calcEdges : Dict ( Int, Int ) Point -> List Edge
calcEdges grid =
    let
        maybeConnect indices point =
            Dict.get indices grid
                |> Maybe.map (\point2 -> { start = point, end = point2 })

        horizontals =
            grid
                |> Dict.map (\( ix, iy ) point -> maybeConnect ( ix + 1, iy ) point)
                |> Dict.toList
                |> List.sortBy (\( ( _, iy ), _ ) -> iy)
                |> List.map Tuple.second
                |> List.filterMap identity

        verticals =
            grid
                |> Dict.map (\( ix, iy ) point -> maybeConnect ( ix, iy + 1 ) point)
                |> Dict.toList
                |> List.sortBy (\( ( ix, _ ), _ ) -> ix)
                |> List.map Tuple.second
                |> List.filterMap identity
    in
    horizontals ++ verticals


perturbGrid : Dict ( Int, Int ) Point -> Dict ( Int, Int ) Point
perturbGrid grid =
    let
        pert =
            puzzle.gridPerturb

        randomPair =
            Random.pair
                (Random.int -pert pert)
                (Random.int -pert pert)

        randomPairListGen =
            Random.list (Dict.size grid) randomPair

        ( randomPairList, _ ) =
            Random.step randomPairListGen puzzle.seed
    in
    Dict.values grid
        |> List.map2 (\( rx, ry ) point -> { x = point.x + rx, y = point.y + ry }) randomPairList
        -- optional: keep borders straight
        |> List.map snapToBorder
        |> List.map2 Tuple.pair (Dict.keys grid)
        |> Dict.fromList


snapToBorder : Point -> Point
snapToBorder { x, y } =
    { x = snapToBorder_ puzzle.gridPerturb params.width x
    , y = snapToBorder_ puzzle.gridPerturb params.height y
    }


snapToBorder_ : Int -> Int -> Int -> Int
snapToBorder_ howClose maxCoord coord =
    if coord - howClose <= 0 then
        0

    else if coord + howClose >= maxCoord then
        maxCoord

    else
        coord



-- DRAWING FUNCTIONS


drawMarker : Point -> Svg msg
drawMarker { x, y } =
    Svg.circle
        [ cx <| String.fromInt x
        , cy <| String.fromInt y
        , r "2"
        , stroke "#666"
        , fillOpacity "0"
        ]
        []


drawEdge : Edge -> Svg msg
drawEdge { start, end } =
    Svg.line
        [ x1 <| String.fromInt start.x
        , y1 <| String.fromInt start.y
        , x2 <| String.fromInt end.x
        , y2 <| String.fromInt end.y
        , strokeWidth "1"
        , stroke "#c66"
        ]
        []


basicTongue : Curve3
basicTongue =
    { start = Point 0 0
    , startControl = Point 70 0
    , middleControl = Point 10 30
    , middle = Point 50 30
    , endControl = Point 30 0
    , end = Point 100 0
    }


drawCurve3 : Curve3 -> Svg msg
drawCurve3 curve =
    let
        m =
            [ "M"
            , String.fromInt curve.start.x
            , String.fromInt curve.start.y
            ]

        c =
            [ "C"
            , String.fromInt curve.startControl.x
            , String.fromInt curve.startControl.y
            , String.fromInt curve.middleControl.x
            , String.fromInt curve.middleControl.y
            , String.fromInt curve.middle.x
            , String.fromInt curve.middle.y
            ]

        s =
            [ "S"
            , String.fromInt curve.endControl.x
            , String.fromInt curve.endControl.y
            , String.fromInt curve.end.x
            , String.fromInt curve.end.y
            ]

        pathString =
            [ m, c, s ]
                |> List.map (String.join " ")
                |> String.join ""
    in
    Svg.path
        [ stroke "black"
        , fill "none"
        , d pathString
        ]
        []



-- CANVAS HELPER


canvas : Int -> Int -> List (Svg msg) -> Svg msg
canvas w h children =
    let
        hStr =
            String.fromInt h

        wStr =
            String.fromInt w

        tileSize =
            10

        xnumtiles =
            w // tileSize

        ynumtiles =
            h // tileSize

        tiles =
            List.lift2 (tile tileSize)
                (List.range 0 <| xnumtiles - 1)
                (List.range 0 <| ynumtiles - 1)
    in
    Svg.svg
        [ width wStr
        , height hStr
        , viewBox <| "0 0 " ++ wStr ++ " " ++ hStr
        ]
        (if puzzle.draftMode then
            Svg.g [] tiles :: children

         else
            children
        )


tile : Int -> Int -> Int -> Svg msg
tile size xc yc =
    let
        col =
            if modBy 2 (xc + yc) == 0 then
                "#eee"

            else
                "#fff"
    in
    Svg.rect
        [ x (String.fromInt (xc * size))
        , y (String.fromInt (yc * size))
        , width (String.fromInt size)
        , height (String.fromInt size)
        , fill col
        ]
        []
