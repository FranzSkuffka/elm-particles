module FRNN exposing ( find )
-- http://efekarakus.github.io/fixed-radius-near-neighbor/

import Dict exposing (..)

type alias Point a =
    { a | x : Float, y : Float }

type alias BucketId =
    ( Int, Int )

type alias FRNN a =
    { points : List (Point a), radius : Float, buckets : Dict BucketId (List (Point a)) }

fromList : Float -> List (Point a) -> FRNN a
fromList radius points =
    let
        folder point accum =
            Dict.update (toKey radius point)
                (\old ->
                    case old of
                        Nothing ->
                            Just [ point ]

                        Just currentPoints ->
                            Just (point :: currentPoints)
                )
                accum

        buckets =
            List.foldl folder Dict.empty points
    in
    { radius = radius, points = points, buckets = buckets }


toKey : Float -> Point a -> ( Int, Int )
toKey radius { x, y } =
    let
        i1 =
            floor (x / radius)

        i2 =
            floor (y / radius)
    in
    ( i1, i2 )


distanceSq : Point a -> Point b -> Float
distanceSq p1 p2 =
    (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y) |> sqrt


getForwardBuckets : BucketId -> Dict BucketId a -> List BucketId
getForwardBuckets ( i1, i2 ) buckets =
    let
        topRight =
            ( i1 + 1, i2 - 1 )

        right =
            ( i1 + 1, i2 )

        bottomRight =
            ( i1 + 1, i2 + 1 )

        bottom =
            ( i1, i2 + 1 )

        potentials =
            [ topRight, right, bottomRight, bottom ]
    in
    List.filter (\bucket -> Dict.member bucket buckets) potentials


pair : BucketId -> FRNN a -> List ( Point a, Point a )
pair bucket { buckets, radius } =
    let
        neighbors : List (Point a)
        neighbors =
            getForwardBuckets bucket buckets
                ++ [ bucket ]
                |> List.filterMap (\b -> Dict.get b buckets)
                |> List.concat
    in
    case Dict.get bucket buckets of
        Nothing ->
            []
        Just points -> pairNodes radius points neighbors

pairNodes radius points neighbors =
  let
    justLinked r point npoint =
      if point == npoint then
          Nothing
      else if distanceSq point npoint < radius then
          Just ( point, npoint )
      else
          Nothing

    uniquePairs point = List.map (\p -> justLinked radius point p) neighbors

  in
    points
      |> List.concatMap uniquePairs
      |> List.filterMap identity


search : FRNN a -> List ( Point a, Point a )
search ({ buckets } as frnn) =
    List.concatMap (\b -> pair b frnn) (Dict.keys buckets)

find : Float -> List (Point a) -> List (Point a, Point a)
find radius points = search (fromList radius points)

