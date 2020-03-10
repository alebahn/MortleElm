module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Canvas exposing (clear, shapes, rect, text)
import Canvas.Settings exposing (Setting, fill)
import Canvas.Settings.Text exposing (font)
import Color
import Json.Decode as Decode

o : Bool
o = False
i : Bool
i = True

levels : Array (Array (Array Bool))
levels = Array.map (\listList -> Array.map Array.fromList (Array.fromList listList)) (Array.fromList
  [
    [
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o],
      [o,o,o,i,i,i,i,i,i,i,i,i,i,i,i,i,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,i,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,i,o,o,o,o,o,o,o,o,o]
    ],
    [
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,i,i,i,i,i,o,i,i,i,i,i,i,i,o,i,i],
      [o,o,i,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o],
      [o,i,i,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o]
    ],
    [
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,i,i,i,i,i,o,o,o],
      [o,i,i,o,o,o,o,o,o,o,o,o,i,o,i,o,o,o,o],
      [o,o,i,i,o,o,o,o,o,o,o,o,i,o,i,o,o,o,o],
      [i,o,i,o,i,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,i,o,o,i,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,i,i,o,o,o,i,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,i,o,o,o,o,i,o,o,o,o,o,o,o,o,o,o,o],
      [i,o,i,o,o,o,o,o,i,o,o,o,o,o,o,o,o,o,o],
      [o,o,i,o,o,o,o,o,o,i,o,o,o,o,o,o,o,o,o],
      [o,i,i,o,o,o,o,o,o,o,i,o,o,o,o,o,o,o,o],
      [o,o,i,o,o,o,o,o,o,o,o,i,o,o,o,o,o,o,o]
    ],
    [
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,i,i,o,i,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,i,i,o,o,o,i,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o,o,o],
      [o,o,o,o,o,i,i,o,o,o,o,o,i,o,o,o,o,o,o],
      [o,o,o,o,o,o,i,o,i,o,i,o,i,o,o,o,o,o,o],
      [o,o,o,i,i,o,i,o,i,o,o,o,i,o,o,o,o,o,o],
      [o,o,o,o,o,o,i,i,i,o,i,o,i,o,o,o,o,o,o],
      [o,i,i,o,o,o,i,o,i,o,i,o,i,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o,o,o]
    ],
    [
      [o,o,o,o,o,o,o,i,o,o,o,o,o,o,o,o,o,o,o],
      [i,i,i,o,o,o,o,i,o,o,o,o,i,i,o,i,o,o,o],
      [o,o,o,o,o,o,o,i,o,o,o,o,i,o,o,i,o,o,o],
      [o,o,o,i,i,i,o,o,o,o,o,o,i,o,i,i,o,o,o],
      [o,o,o,o,o,i,o,o,o,o,o,o,i,o,o,i,o,o,o],
      [o,i,i,o,o,i,i,i,i,i,i,o,i,i,o,i,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,i,o,i,o,o,i,o,o,o],
      [o,o,o,o,i,i,o,o,o,o,i,o,i,o,i,i,o,o,o],
      [o,o,o,o,i,o,o,o,o,o,i,o,o,o,o,i,o,o,o],
      [o,o,o,o,i,o,i,i,i,o,i,i,i,i,o,i,o,o,o],
      [o,i,o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,i,i],
      [o,i,o,o,i,i,o,o,o,o,o,o,o,o,o,i,o,o,o]
    ],
    [
      [i,i,i,i,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,i,o,i,i,o,o,o,o,o,o,o,o,o,o,o,o],
      [i,i,o,i,o,o,o,o,o,i,i,o,o,o,o,o,o,o,o],
      [o,o,o,i,o,o,o,o,o,i,o,o,o,i,i,o,o,o,o],
      [o,i,i,i,i,i,i,o,o,i,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,i,o,o,i,o,o,o,o,o,o],
      [i,i,i,o,o,o,o,o,o,i,o,o,i,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,i,o,i,o,o,i,o,o,o,o,o,o],
      [o,o,o,i,i,i,i,o,o,i,o,o,o,o,o,i,o,i,i],
      [i,i,o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o]
    ],
    [
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,i,i,i,i,i,i,i,i,i,i,o],
      [o,o,o,o,o,o,o,i,i,o,o,o,o,o,o,o,o,o,o],
      [o,i,i,i,i,i,i,i,i,o,o,i,i,i,i,i,i,i,i],
      [o,o,o,o,o,o,o,o,i,o,o,i,o,o,o,o,o,o,o],
      [i,o,o,o,o,o,o,i,i,o,o,i,o,o,o,i,o,o,o],
      [i,i,i,i,i,i,o,o,i,o,i,i,i,i,o,i,o,o,o],
      [o,o,o,o,o,i,i,o,i,o,o,i,o,o,o,i,o,o,o],
      [o,o,o,o,o,i,o,o,i,o,o,i,o,i,i,i,o,o,o],
      [o,o,i,i,o,i,o,i,i,o,o,i,o,o,o,i,o,o,o],
      [i,o,i,o,o,o,o,o,i,i,o,i,i,i,o,i,o,o,o],
      [o,o,i,o,o,o,i,o,i,o,o,o,o,o,o,i,o,o,o]
    ],
    [
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
      [o,o,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,o,i],
      [o,o,o,i,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o],
      [i,i,o,i,o,i,i,i,i,i,i,i,i,i,o,o,i,o,o],
      [o,o,o,i,o,i,o,o,o,o,o,o,o,i,i,o,i,i,o],
      [o,i,o,i,o,i,i,o,i,i,i,i,o,i,o,o,o,i,o],
      [o,o,o,i,o,i,o,o,i,o,o,o,o,i,i,o,o,i,o],
      [o,i,i,i,o,i,o,i,i,o,i,o,o,i,o,o,o,o,o],
      [o,o,o,i,o,i,o,o,i,o,i,i,i,i,i,i,i,i,i],
      [i,o,o,i,o,i,o,i,i,o,o,o,o,o,o,o,o,o,o],
      [o,o,o,i,o,o,o,o,i,o,o,o,o,o,o,o,o,o,o]
    ],
    [
      [i,i,o,i,o,i,i,i,i,i,o,i,o,i,i,i,i,i,o],
      [o,o,o,i,o,o,o,o,o,o,o,i,o,o,o,o,o,o,o],
      [o,i,i,i,i,i,o,i,o,i,i,i,i,i,o,i,o,i,i],
      [o,o,o,i,o,o,o,i,o,o,o,i,o,o,o,i,o,o,o],
      [i,i,o,i,o,i,i,i,i,i,o,i,o,i,i,i,i,i,o],
      [o,o,o,i,o,o,o,i,o,o,o,i,o,o,o,i,o,o,o],
      [o,i,i,i,i,i,o,i,o,i,i,i,i,i,o,i,o,i,i],
      [o,o,o,i,o,o,o,i,o,o,o,i,o,o,o,i,o,o,o],
      [i,i,o,i,o,i,i,i,i,i,o,i,o,i,i,i,i,i,o],
      [o,o,o,i,o,o,o,i,o,o,o,i,o,o,o,i,o,o,o],
      [o,i,i,i,i,i,o,i,o,i,i,i,i,i,o,i,o,i,i],
      [o,o,o,o,o,o,o,i,o,o,o,o,o,o,o,i,o,o,o]
    ],
    [
      [i,o,o,i,o,o,o,o,o,i,o,o,o,o,o,i,o,o,i],
      [o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o],
      [o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o],
      [i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i],
      [o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o],
      [o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o],
      [i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,o],
      [o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o],
      [o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o],
      [i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i],
      [o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o],
      [o,i,o,o,o,o,o,i,o,o,o,o,o,i,o,o,o,o,o]
    ]
  ])

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
    Aim _ -> onKeyDown (Decode.map keyToAimMsg (Decode.field "key" Decode.string))

keyToMenuMsg : String -> Msg
keyToMenuMsg key =
  if key == "ArrowUp" then
    MenuUp
  else if key == "ArrowDown" then
    MenuDown
  else if key == "ArrowRight" || key == "Enter" || key == "Enter" || key == " " then
    MenuAccept
  else
    KeyOther

keyToAimMsg : String -> Msg
keyToAimMsg key = KeyOther

-- MODEL
type alias Model = { state : GameState
                   , startLevel : Int
                   }
type GameState = Menu MenuSelection
               | Aim AimModel
type MenuSelection = NewGame | Continue
type alias AimModel = { level : Int }

init : () -> (Model, Cmd Msg)
init _ =
  (Model (Menu NewGame) 0, Cmd.none)

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
       MenuAccept -> ({ model | state = Aim (AimModel model.startLevel) }, Cmd.none)
       _ -> (model, Cmd.none)
    Aim aimModel -> (model, Cmd.none)

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
    Aim aimModel -> drawLevel aimModel.level

drawLevel : Int -> List Canvas.Renderable
drawLevel levelNum = case Array.get levelNum levels of
   Nothing -> []
   Just level -> Array.foldl (++) [] (Array.indexedMap drawRow level)

drawRow : Int -> Array Bool -> List Canvas.Renderable
drawRow rowNum row = Array.foldl (++) [] (Array.indexedMap (drawCell rowNum) row)

drawCell : Int -> Int -> Bool -> List Canvas.Renderable
drawCell rowNum colNum cell =
  if cell then
    [shapes [fill Color.black ] [ rect (toFloat colNum * 5, toFloat rowNum * 5) 5 5] ]
  else
    []

foreground : Model -> List Canvas.Renderable
foreground model =
  case model.state of
     Menu menuSelection -> [ text [ gameFont ] (72, getYCoordinateFromMenuSelection menuSelection) "<" ]
     Aim aimModel -> []

getYCoordinateFromMenuSelection : MenuSelection -> Float
getYCoordinateFromMenuSelection menuSelection =
  case menuSelection of
    NewGame -> 24
    Continue -> 32

clearScreen : Canvas.Renderable
clearScreen = clear ( 0, 0 ) (toFloat width) (toFloat height)