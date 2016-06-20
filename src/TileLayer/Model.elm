module TileLayer.Model exposing (..)

import UrlTemplate exposing (getTileUrl)
import Geometry.Point as Point exposing (Point)
import Geometry.Bounds as Bounds exposing (Bounds)
import Geo.LatLng as LatLng exposing (LatLng)
import Debug


type alias TileLayer =
    { url : String
    , options : Options
    , pixelBounds : Bounds
    , origin : Point
    , zoom : Float
    , tiles : List Point
    }


type alias Subdomains =
    String


type alias Options =
    { tileSize : Float
    , minZoom : Float
    , maxZoom : Float
    , subdomains : Subdomains
    }


defaultOptions : Options
defaultOptions =
    Options 256
        0
        18
        "abc"


new : String -> Maybe Options -> Bounds -> Point -> Float -> TileLayer
new url mbOpts bounds origin zoom =
    let
        opts =
            Maybe.withDefault defaultOptions mbOpts
    in
        TileLayer url opts bounds origin zoom (loadTiles bounds origin zoom opts.tileSize)


floorFloat : Float -> Float
floorFloat x =
    toFloat <| floor x


loadTiles : Bounds -> Point -> Float -> Float -> List Point
loadTiles pixelBounds origin zoom tileSize =
    let
        nwTilePoint =
            Point.new (floorFloat (pixelBounds.min.x / tileSize))
                (floorFloat (pixelBounds.min.y / tileSize))

        seTilePoint =
            Point.new (floorFloat (pixelBounds.max.x / tileSize))
                (floorFloat (pixelBounds.max.y / tileSize))

        xRange =
            Debug.log "xRange" [nwTilePoint.x..seTilePoint.x]

        yRange =
            Debug.log "yRange" [nwTilePoint.y..seTilePoint.y]

        loadTile y =
            List.map (\x -> Point.new x y) xRange
    in
        List.map loadTile yRange
            |> List.concat
