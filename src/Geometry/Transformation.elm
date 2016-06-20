module Geometry.Transformation exposing (..)

import Geometry.Point as Point exposing (Point)


type alias TransformationBase =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    }


type alias Transformation =
    { transform : Point -> Maybe Float -> Point
    , untransform : Point -> Maybe Float -> Point
    }


new : Float -> Float -> Float -> Float -> Transformation
new a b c d =
    let
        tb =
            TransformationBase a b c d
    in
        Transformation (transform tb) (untransform tb)


transform : { a : Float, b : Float, c : Float, d : Float } -> Point -> Maybe Float -> Point
transform { a, b, c, d } point mbScale =
    let
        scale =
            Maybe.withDefault 1 mbScale
    in
        Point.new (scale * (a * point.x + b))
            (scale * (c * point.y + d))


untransform : { a : Float, b : Float, c : Float, d : Float } -> Point -> Maybe Float -> Point
untransform { a, b, c, d } point mbScale =
    let
        scale =
            Maybe.withDefault 1 mbScale
    in
        Point.new ((point.x - b) / a / scale)
            ((point.y - d) / c / scale)
