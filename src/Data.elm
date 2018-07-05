module Data exposing (Node, Link, Settings)

type alias Node = {
      x: Float
    , y: Float
    , vx: Float
    , vy: Float
  }

type alias Link = {
      from: Node
    , to: Node
    , opacity: Float
  }

type alias Settings = { maxDist : Float, maxVelocity : Float }
