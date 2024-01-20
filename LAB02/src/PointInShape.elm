
module PointInShape exposing (..)

type alias Point = {x: Float, y: Float}
type Shape2D
  = Circle2 {center: Point, radius: Float}
  | Rectangle2 {topLeftCorner: Point, bottomRightCorner: Point}
  | Triangle2 {pointA: Point, pointB: Point, pointC: Point}

