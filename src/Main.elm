module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class)
import Canvas exposing (shapes, rect, text)
import Canvas.Settings exposing (Setting, fill)
import Canvas.Settings.Text exposing (font)
import Color

-- MAIN

main : Program () Model Msg
main =
  Browser.element
  { init = init
  , subscriptions = subscriptions
  , update = update, view = view }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- MODEL
type GameState = Menu
type alias Model = { state: GameState }


init : () -> (Model, Cmd Msg)
init _ =
  (Model Menu, Cmd.none)



-- UPDATE


type Msg
  = Increment
  | Decrement
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

-- VIEW
width : Int
width = 95
height: Int
height = 60

view : Model -> Html Msg
view model =
  Canvas.toHtml
      ( width, height )
      [ class "gameCanvas"]
      (background model)

gameFont : Setting
gameFont = font {size = 8, family = "'Press Start 2P'"}

background : Model -> List Canvas.Renderable
background model =
  case model.state of
     Menu -> [ text [ gameFont ] (24, 12) "MORTLE"
             , text [ gameFont ] (4, 24) "New Game"
             , text [ gameFont ] (4, 32) "Continue" ]

  --setFont context "8px 'Press Start 2P'"
  --fillText context "MORTLE" 24.0 12.0
  --fillText context "New Game" 4.0 24.0
  --fillText context "Continue" 4.0 32.0

clearScreen : Canvas.Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]