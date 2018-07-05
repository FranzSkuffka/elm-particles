module Particles exposing (render, randomNodes, animationSub, calcNextNodePositions)

import Display exposing ( render )
import DataOperations
import AnimationFrame
import Time

randomNodes = DataOperations.randomNodes

render = Display.render

animationSub nextanimationstep = AnimationFrame.diffs <| Time.inMilliseconds >> nextanimationstep

calcNextNodePositions = DataOperations.calcNextNodeState
