module Geo.LatLng exposing (..)


type alias LatLng =
    { lat : Float
    , lng : Float
    }



-- Clamp Latitude into -90..90


clampLat : Float -> Float
clampLat lat =
    max (min lat 90) -90



-- Wrap Longtitude into -180..180


wrapLng : Float -> Float
wrapLng lng =
    let
        wrapped =
            if lng < -180 then
                180
            else
                -180

        roundedLng =
            ((round lng) + 180) % 360
    in
        (roundedLng + wrapped) |> toFloat


equals : LatLng -> LatLng -> Bool
equals one two =
    let
        m =
            max (abs <| one.lat - two.lat) (abs <| one.lng - two.lng)
    in
        m <= margin


new : Float -> Float -> LatLng
new lat lng =
    LatLng (clampLat lat) (wrapLng lng)


degToRad : Float -> Float
degToRad deg =
    pi / 180 * deg


radToDeg : Float -> Float
radToDeg rad =
    180 / pi * rad


margin =
    1.0e-9
