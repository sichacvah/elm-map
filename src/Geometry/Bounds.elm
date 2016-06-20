module Geometry.Bounds exposing (..)

import Geometry.Point as Point exposing (Point)


type alias Bounds =
    { min : Point
    , max : Point
    }


new : Point -> Point -> Bounds
new min max =
    Bounds (Point.new min.x min.y) (Point.new max.x max.y)
