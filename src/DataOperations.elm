module DataOperations exposing (randomNodes, calcNextNodeState)
import Data exposing ( Node )
import Random

randomNode settings (width, height) =
  let
    randomX = Random.float 0 <| toFloat width
    randomY = Random.float 0 <| toFloat height
    randomVX = Random.float -settings.maxVelocity settings.maxVelocity
    randomVY = Random.float -settings.maxVelocity settings.maxVelocity
  in
    Random.pair 
      (Random.pair randomX randomY)
      (Random.pair randomVX randomVY)

-- helpers for deconstructing the result of a random command
x = Tuple.first >> Tuple.first
y = Tuple.first >> Tuple.second
vx = Tuple.second >> Tuple.first
vy = Tuple.second >> Tuple.second

randomNodes msg settings (width, height) count =
  let
    gen = randomNode settings (width, height)
    msgConstructor = \nodeData -> Node (x nodeData) (y nodeData) (vx nodeData) (vy nodeData) |> msg
    oneRandomNode = Random.generate msgConstructor gen
  in
    Cmd.batch <| List.repeat count oneRandomNode

calcNextNodeState dimensions ms nodes = List.map (nextNodePosition dimensions ms) nodes

reoccur : Float -> Float -> Float
reoccur max val = val
  |> (\val -> if val > max then 0 else val)
  |> (\val -> if val < 0 then max else val)

nextNodePosition (width, height) ms node = { node
    | x = (reoccur <| toFloat width) <| node.x + (node.vx) * ms / 1000
    , y = (reoccur <| toFloat height) <| node.y + (node.vy) * ms / 1000
  }
