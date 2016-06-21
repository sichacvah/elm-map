module Layout.Layout exposing (..)

import Geometry.Point as Point exposing (Point)
import Geometry.Transformation as Transformation exposing (Transformation)


type alias Layout =
    { transformation : Transformation
    , size : Point
    , origin : Point
    }
