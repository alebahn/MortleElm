module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Canvas exposing (clear, shapes, rect, text)
import Canvas.Settings exposing (Setting, fill)
import Canvas.Settings.Text exposing (font)
import Color
import Json.Decode as Decode

-- MAIN

main : Program () Model Msg
main =
  Browser.element
  { init = init
  , subscriptions = subscriptions
  , update = update, view = view }

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
    Menu _ -> onKeyDown (Decode.map keyToMenuMsg (Decode.field "key" Decode.string))

keyToMenuMsg : String -> Msg
keyToMenuMsg key =
  if key == "ArrowUp" then
    MenuUp
  else if key == "ArrowDown" then
    MenuDown
  else
    KeyOther


-- MODEL
type MenuSelection = NewGame | Continue
type GameState = Menu MenuSelection
type alias Model = { state : GameState }

init : () -> (Model, Cmd Msg)
init _ =
  (Model (Menu NewGame), Cmd.none)

-- UPDATE

type Msg
  = MenuDown
  | MenuUp
  | MenuAccept
  | KeyOther

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model.state of
    Menu _ -> case msg of
       MenuUp -> ({ model | state = Menu NewGame }, Cmd.none)
       MenuDown -> ({ model | state = Menu Continue }, Cmd.none)
       _ -> (model, Cmd.none)

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
      (background model ++ foreground model)

gameFont : Setting
gameFont = font {size = 8, family = "'Press Start 2P'"}

background : Model -> List Canvas.Renderable
background model =
  clearScreen ::
  case model.state of
     Menu _ -> [ text [ gameFont ] (24, 12) "MORTLE"
               , text [ gameFont ] (4, 24) "New Game"
               , text [ gameFont ] (4, 32) "Continue" ]

foreground : Model -> List Canvas.Renderable
foreground model =
  case model.state of
     Menu menuSelection -> [ text [ gameFont ] (72, getYCoordinateFromMenuSelection menuSelection) "<" ]

getYCoordinateFromMenuSelection : MenuSelection -> Float
getYCoordinateFromMenuSelection menuSelection =
  case menuSelection of
    NewGame -> 24
    Continue -> 32

clearScreen : Canvas.Renderable
clearScreen = clear ( 0, 0 ) (toFloat width) (toFloat height)