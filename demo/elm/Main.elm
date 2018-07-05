module Main exposing ( main )
import Html exposing ( .. )

main = Html.program { init = (model, Cmd.none), view = view, update = update, subscriptions = subscriptions}

subscriptions model = Sub.none

update model msg = (model, Cmd.none)


type alias Model = Float

model : Model
model = 0

view : Model -> Html msg
view model = text "Hello World"
