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
        cnvs =
            canvas params.width params.height

        grid =
            rectangularGrid puzzle.piecesX puzzle.piecesY
                |> perturbGrid

        markers =
            Dict.values grid
                |> List.map drawMarker

        isOnBorder edge =
            False
                || (edge.start.x == 0 && edge.end.x == 0)
                || (edge.start.y == 0 && edge.end.y == 0)
                || (edge.start.x == params.width && edge.end.x == params.width)
                || (edge.start.y == params.height && edge.end.y == params.height)

        edges =
            calcEdges grid
                |> List.filter (not << isOnBorder)

        ( flips, _ ) =
            Random.uniform True [ True, False ]
                |> Random.list (List.length edges)
                |> (\l -> Random.step l puzzle.seed)

        tongues =
            List.map2 makeTongue flips edges

        border =
            Svg.rect
                [ x "0"
                , y "0"
                , width (String.fromInt params.width)
                , height (String.fromInt params.height)
                , fillOpacity "0"
                , stroke "black"
                ]
                []
    in
    cnvs <|
        if puzzle.draftMode then
            [ Svg.g [] <| List.map drawCurve3 tongues
            , border
            , Svg.g [] markers
            , Svg.g [] <| List.map drawEdge edges
            ]

        else
            [ Svg.g [] <| List.map drawCurve3 tongues
            , border
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


makeTongue : Bool -> Edge -> Curve3
makeTongue flip { start, end } =
    let
        vEdge =
            { x = end.x - start.x
            , y = end.y - start.y
            }

        vPerp =
            if start.y /= end.y then
                { x = 1
                , y = toFloat -vEdge.x / toFloat vEdge.y |> round
                }

            else
                --if start.x /= end.x
                { x = toFloat -vEdge.y / toFloat vEdge.x |> round
                , y = 1
                }

        flipMult =
            if flip then
                -1

            else
                1

        vPerpN =
            { x = flipMult * toFloat vPerp.x / norm vPerp |> round
            , y = flipMult * toFloat vPerp.y / norm vPerp |> round
            }

        middleScale =
            toFloat puzzle.pixelsPerCell * 0.18 |> round

        scaleV h vect =
            { x = toFloat vect.x * h |> round
            , y = toFloat vect.y * h |> round
            }

        scale h n =
            toFloat n * h |> round

        middle =
            { x = (vPerpN.x * middleScale) + scale 0.5 (start.x + end.x)
            , y = (vPerpN.y * middleScale) + scale 0.5 (start.y + end.y)
            }
    in
    { start = Point start.x start.y
    , startControl = Point (start.x + scale 0.8 vEdge.x) (start.y + scale 0.8 vEdge.y)
    , middleControl = Point (middle.x - scale 0.4 vEdge.x) (middle.y - scale 0.4 vEdge.y)
    , middle = Point middle.x middle.y
    , endControl = Point (end.x - scale 0.8 vEdge.x) (end.y - scale 0.8 vEdge.y)
    , end = Point end.x end.y
    }


norm : Point -> Float
norm vect =
    (vect.x ^ 2 + vect.y ^ 2)
        |> toFloat
        |> sqrt



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
            [ curve.start.x
            , curve.start.y
            ]
                |> List.map String.fromInt

        c =
            [ curve.startControl.x
            , curve.startControl.y
            , curve.middleControl.x
            , curve.middleControl.y
            , curve.middle.x
            , curve.middle.y
            ]
                |> List.map String.fromInt

        s =
            [ curve.endControl.x
            , curve.endControl.y
            , curve.end.x
            , curve.end.y
            ]
                |> List.map String.fromInt

        pathString =
            [ "M" :: m, "C" :: c, "S" :: s ]
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
