module Models exposing (..)

import Geo.Projection as Projection exposing (Projection)
import Geometry.Transformation as Transformation exposing (Transformation)
import Geo.LatLng as LatLng exposing (LatLng)
import Geometry.Point as Point exposing (Point)
import Geometry.Bounds as Bounds exposing (Bounds)
import Bitwise exposing (shiftLeft)
import TileLayer.Model as TL exposing (TileLayer)
import Debug


type alias Model =
    { projection : Projection
    , transformation : Transformation
    , scaling : Float -> Float
    , center : LatLng
    , zoom : Float
    , layers : List TileLayer
    , position : Point
    , size : Point
    , url : String
    }


new : Float -> Float -> Model
new width height =
    let
        initialModel =
            init width height
    in
        Debug.log "MODEL " { initialModel | layers = [ TL.new initialModel.url Nothing (getPixelBounds initialModel) (getPixelOrigin initialModel) initialModel.zoom ] }


init : Float -> Float -> Model
init width height =
    Model (Projection.mercator)
        (Transformation.new (0.5 / pi) 0.5 (-0.5 / pi) 0.5)
        (\zoom -> toFloat <| 256 * (1 `shiftLeft` (round zoom)))
        (Debug.log "LatLng " (LatLng.new 69 0))
        5
        []
        (Point.new 0 0)
        (Point.new width height)
        "http://{s}.tile.osm.org/{z}/{x}/{y}.png"


getPixelBounds : Model -> Bounds
getPixelBounds model =
    let
        topLeftPoint =
            getTopLeftPoint model
    in
        Debug.log " PIXEL BOUNDS " (Bounds.new (topLeftPoint) (Point.add topLeftPoint model.size))


getTopLeftPoint : Model -> Point
getTopLeftPoint model =
    Debug.log "TOP LEFT POINT"
        (Point.subtract (project model model.center model.zoom)
            (Point.divideBy model.size 2)
        )


getPixelOrigin : Model -> Point
getPixelOrigin model =
    Debug.log " PIXEL ORIGIN" (getTopLeftPoint model)


project : Model -> LatLng -> Float -> Point
project model coord zoom =
    let
        projectedPoint =
            (model.projection.project coord)

        scale =
            model.scaling zoom
    in
        Debug.log "PROJECTED Point" (model.transformation.transform projectedPoint (Just scale))
