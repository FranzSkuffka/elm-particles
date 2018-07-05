module Main exposing ( main )
import Html exposing ( .. )
import Html.Attributes exposing ( style )

import Particles
import Data
import DataOperations

main = Html.program { init = (model, initialCmd), view = view, update = update, subscriptions = subscriptions}

-- create 100 random nodes
initialCmd = Particles.randomNodes RandomNodeGenerated settings dimensions 100

subscriptions model = Particles.animationSub NextAnimationStep

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  RandomNodeGenerated node -> (
    {model | nodes = node :: model.nodes},
  Cmd.none)
  NextAnimationStep diff -> (
    {model | nodes = Particles.calcNextNodePositions dimensions diff model.nodes},
  Cmd.none)

type Msg =
    RandomNodeGenerated Data.Node
  | NextAnimationStep Float

type alias Model = {
  nodes : List Data.Node,
  ms : Float
}

model : Model
model = { nodes = [], ms = 0 }

dimensions = (1400, 1000)
settings = {
    maxDist = 130,
    maxVelocity = 10
  }

view : Model -> Html msg
view model =
  let
    attribs = [ style [("background", "black")] ]
    viz = [Particles.render settings dimensions model.nodes]
  in div attribs viz
