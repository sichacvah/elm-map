module Geometry.Point exposing (..)


{-| Point coords -}
type alias PointCoords number = (number, number)
type alias IntPointCoords = PointCoords Int
type alias FloatPointCoords = FloatPointCoords Float

{-| Generic point coordinates -}
type Point
  = IntPoint IntPointCoords
  | FloatPoint FloatPointCoords


{-| Get point in float coordinates -}
toFloat' : Point -> Point
toFloat' point =
  case point of
    IntPoint (x, y) -> FloatPoint (toFloat x, toFloat y)
    _ -> point

{-| Get point in int coordinates -}
toInt : Point -> Point
toInt point =
  case point of
    FloatPoint (x, y) -> IntPoint (round x, round y)
    _ -> point

{-| Get horizontal coord of point in Float -}
x : Point -> Float
x point =
  case point of 
    IntPoint (x, _) -> toFloat x
    FloatPoint (x, _) -> x


{-| Get horizontal coord of point in Int -}
intX : Point -> Int
intX point =
    case point of
      IntPoint (x, _) -> toFloat x
      FloatPoint (x, _) -> round x


{-| Get vertical coord of point in Float -}
y : Point -> Float
y point =
  case point of 
    IntPoint (_, y) -> toFloat y
    FloatPoint (_, y) -> y


{-| Get vertical coord of point in Int -}
intY : Point -> Int
intY point =
    case point of
      IntPoint (_, y) -> toFloat y
      FloatPoint (_, y) -> round y


{-| Build Point object from Float coordinates 

  floatFactory (2.5, 3.0) === FloatPointCoords (2.5, 3.0)
-}
floatFactory : (Float, Float) -> Point
floatFactory (x, y) =
  FloatPointCoords (x, y)

{-| Build Point object from Int coordinates 

  intFactory (2, 3) === IntPointCoords (2, 3)
-}
intFactory : (Int, Int) -> Point
intFactory (x,y) =
  IntPointCoords (x, y)

{-| Addiction for Points
    IntPointCoords (1, 2) `add` IntPointCoords (2, 3) === IntPointCoords (3, 5)
-}
add : Point -> Point -> Point
add a b =
  case a of
    IntPoint (ax, ay) ->
      case b of 
        IntPoint (bx, by) -> IntPointCoords (ax + bx, ay + by)
        FloatPoint (bx, by) ->
          let
              _ax = toFloat ax
              _ay = toFloat ay
          in
              FloatPointCoords (_ax + bx, _ay + by)
    FloatPoint (ax, ay) ->
      case b of
        FloatPoint (bx, by) -> FloatPointCoords (ax + bx, ay + by)
        IntPoint (bx, by) ->
          let
              _bx = toFloat bx
              _by = toFloat by
          in
              FloatPointCoords (ax + _bx, ay + _by)

infixr 4 `add`


{-| Subtraction for Points
    IntPointCoords (1, 2) =-- IntPointCoords (2, 3) =-- IntPointCoords (-1, -1)
-}
sub : Point -> Point -> Point 
sub a b =
  case a of 
    IntPoint (ax, ay) ->
      case b of
        IntPoint (bx, by) -> IntPointCoords (ax + bx, ay + by)
        FloatPoint (bx, by) -> 
          let 
              _ax = toFloat ax
              _ay = toFloat ay
          in
              FloatPointCoords (_ax + bx, _ay + by)

    FloatPoint (ax, ay) ->
      case b of
        FloatPoint (bx, by) -> FloatPointCoords (ax + bx, ay + by)
        IntPoint (bx, by) ->
          let
              _bx = toFloat bx
              _by = toFloat by

          in
              FloatPointCoords (ax + _bx, ay + _by)





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
