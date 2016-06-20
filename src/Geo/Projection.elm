module Geo.Projection exposing (..)

import Geo.LatLng as LatLng exposing (LatLng)
import Geometry.Point as Point exposing (Point)
import Geometry.Transformation as Transformation exposing (Transformation)


type alias Projection =
    { project : LatLng -> Point
    , unproject : Point -> LatLng
    }


maxLat : Float
maxLat =
    let
        a =
            e ^ (2 * pi)
    in
        (asin (a - 1) / (a + 1)) |> LatLng.radToDeg


project : LatLng -> Point
project latlng =
    let
        lat =
            max (min maxLat latlng.lat) -maxLat

        x =
            LatLng.degToRad latlng.lng

        y =
            LatLng.degToRad lat
    in
        Point.new x (logBase e (pi / 4 + y / 2))


unproject : Point -> LatLng
unproject point =
    let
        lng =
            LatLng.radToDeg point.x

        lat =
            LatLng.radToDeg (2 * (atan <| e ^ point.y) - pi / 2)
    in
        LatLng.new lat lng


mercator : Projection
mercator =
    Projection project unproject
