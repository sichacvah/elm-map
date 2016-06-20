module TileLayer.View exposing (..)

import UrlTemplate exposing (getTileUrl)
import TileLayer.Messages exposing (Msg)
import TileLayer.Model exposing (TileLayer)
import Html exposing (..)
import Html.Attributes exposing (class, style, src)
import Geometry.Point as Point exposing (Point)
import Bitwise exposing (shiftLeft)


view : String -> TileLayer -> Html.Html Msg
view url tileLayer =
    let
        tileLimit =
            1 `shiftLeft` (round tileLayer.zoom) |> toFloat
    in
        div [ class "map-layer" ]
            (List.map (tileImage url tileLayer)
                (List.filter (\{ y } -> y >= 0 && y < tileLimit) tileLayer.tiles)
            )


floorFloat : Float -> Float
floorFloat x =
    toFloat <| floor x


tileImage : String -> TileLayer -> Point -> Html Msg
tileImage url tileLayer tilePoint =
    let
        origin =
            tileLayer.origin

        tileSize =
            tileLayer.options.tileSize

        tilePos =
            Point.subtract (Point.multipleBy tilePoint tileSize) origin

        zoom =
            tileLayer.zoom

        tileLimit =
            1 `shiftLeft` (floor zoom)

        x =
            toFloat ((((round tilePoint.x) % tileLimit) + tileLimit) % tileLimit)

        y =
            tilePoint.y
    in
        img
            [ style
                [ ( "left", (toString tilePos.x) ++ "px" )
                , ( "top", (toString tilePos.y) ++ "px" )
                , ( "position", "absolute" )
                ]
            , src (getTileUrl (Point.new x y) zoom "abc" url)
            ]
            []
