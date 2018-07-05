module Display exposing ( render )

import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Data exposing ( .. )
import FRNN

render : Settings -> (Int, Int) -> List Node -> Svg msg
render settings (screenWidth, screenHeight) nodes =
  let
    links = FRNN.find settings.maxDist nodes
      |> List.map (\pair -> Link (Tuple.first pair) (Tuple.second pair) 0)
  in
    svg [ width (toString screenWidth ++ "px"), height (toString screenHeight ++ "px") ]
        [ g [ class "links" ] <| List.map linkElement <| determineLinkOpacity settings <| links
        , g [ class "nodes" ] <| List.map nodeElement <| nodes
        ]

determineLinkOpacity : Settings -> List Link -> List Link
determineLinkOpacity settings =
  List.map (\link -> { link | opacity = (settings.maxDist - len link) / (settings.maxDist + 30) })

len link = dist link.from link.to

dist : Node -> Node -> Float
dist a b = (a.x - b.x)^2 + (a.y - b.y)^2
  |> sqrt

getLinks : List Node -> List Link -> List Link
getLinks nodes linkAccumulator =
    case nodes of
        [] ->
            linkAccumulator
        [node] ->
            linkAccumulator
        head :: tail ->
            linkAccumulator -- everything calculated so far, starts with empty list
                |> thoseLinks head tail -- add links for this node to accum
                |> getLinks tail -- get rest links

thoseLinks : Node -> List Node -> List Link -> List Link
thoseLinks thisNode otherNodes linkAccumulator =
    List.foldl (\otherNode accum -> Link thisNode otherNode 0 :: accum) linkAccumulator otherNodes

linkElement edge =
  line
      [ strokeWidth "2"
      , stroke "#fff"
      , opacity <| toString edge.opacity
      , x1 (toString edge.from.x)
      , y1 (toString edge.from.y)
      , x2 (toString edge.to.x)
      , y2 (toString edge.to.y)
      ]
      []


nodeElement node =
    circle
        [ r "1.5"
        , fill "#fff"
        , opacity ".5"
        , stroke "transparent"
        , strokeWidth "7px"
        , cx (toString node.x)
        , cy (toString node.y)
        ]
        [  ]

