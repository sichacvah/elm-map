module Geometry.Point exposing (..)

{-| Point coords
-}


type alias PointCoords number =
    ( number, number )


type alias IntPointCoords =
    PointCoords Int


type alias FloatPointCoords =
    FloatPointCoords Float


type alias Point =
    { x : Float
    , y : Float
    }


round' : Point -> Point
round' point =
    let
        x =
            round point.x |> toFloat

        y =
            round point.y |> toFloat
    in
        Point x y


add : Point -> Point -> Point
add point1 point2 =
    new (point1.x + point2.x) (point1.y + point2.y)


subtract : Point -> Point -> Point
subtract point1 point2 =
    new (point1.x - point2.x) (point1.y - point2.y)


divideBy : Point -> Float -> Point
divideBy point num =
    new (point.x / num) (point.y / num)


multipleBy : Point -> Float -> Point
multipleBy point num =
    new (point.x * num) (point.y * num)
