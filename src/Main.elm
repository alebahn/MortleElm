module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown, onAnimationFrame)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Canvas exposing (clear, shapes, rect, text, Point)
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

-- CONSTANTS

aimLength : Float
aimLength = 6

angleDelta : Float
angleDelta = pi / 32

airResistance : Float
airResistance = 0.9756

gravity : Float
gravity = 0.025

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
    Launch _ -> onAnimationFrame (\_ -> Frame)

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
keyToAimMsg key =
  if key == "ArrowLeft" then
    AimLeft
  else if key == "ArrowRight" then
    AimRight
  else if key == "ArrowUp" || key == "Enter" || key == "Enter" || key == " " then
    AimLaunch
  else
    KeyOther

-- MODEL
type alias Model = { state : GameState
                   , startLevel : Int
                   }
type GameState = Menu MenuSelection
               | Aim AimModel
               | Launch LaunchModel
type MenuSelection = NewGame | Continue
type alias AimModel = { level : Int
                      , currentPosition : Point
                      , aimAngle : Float
                      }
type alias LaunchModel = { level : Int
                         , currentPosition : Point
                         , velocityX : Float
                         , velocityY : Float
                         , aimAngle : Float
                         }

init : () -> (Model, Cmd Msg)
init _ =
  (Model (Menu NewGame) 0, Cmd.none)

-- UPDATE

type Msg
  = MenuDown
  | MenuUp
  | MenuAccept
  | AimLeft
  | AimRight
  | AimLaunch
  | KeyOther
  | Frame

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model.state of
    Menu _ -> case msg of
       MenuUp -> ({ model | state = Menu NewGame }, Cmd.none)
       MenuDown -> ({ model | state = Menu Continue }, Cmd.none)
       MenuAccept -> ({ model | state = Aim (AimModel model.startLevel (2, 58) 0) }, Cmd.none)
       _ -> (model, Cmd.none)
    Aim aimModel -> case msg of
      AimLeft -> ({ model | state = Aim { aimModel | aimAngle = aimModel.aimAngle - angleDelta }}, Cmd.none)
      AimRight -> ({ model | state = Aim { aimModel | aimAngle = aimModel.aimAngle + angleDelta }}, Cmd.none)
      AimLaunch -> ({ model | state = Launch (LaunchModel aimModel.level aimModel.currentPosition (sin aimModel.aimAngle) -(cos aimModel.aimAngle) aimModel.aimAngle) }, Cmd.none)
      _ -> (model, Cmd.none)
    Launch launchModel -> case msg of
      Frame ->
        let
          (currentX, currentY) = launchModel.currentPosition
          level = launchModel.level
        in
          if currentX >= 90 && currentY >= 55 then
            (advanceToNextLevel model launchModel, Cmd.none)
          else if checkCollision level (currentX, currentY + 1) then
            ({ model | state = Aim (AimModel level (roundPoint (currentX, currentY - 1)) launchModel.aimAngle) }, Cmd.none)
          else
            let
              launchModelPrime =
                if (checkCollision level (currentX, currentY - 1) && launchModel.velocityY < 0) then
                  { launchModel | velocityX = 0, velocityY = 0 }
                else if ((checkCollision level (currentX - 1, currentY)) && launchModel.velocityX < 0 ) ||
                        ((checkCollision level (currentX + 1, currentY)) && launchModel.velocityX > 0) then
                  { launchModel | velocityX = -launchModel.velocityX }
                else launchModel
            in
              ({ model | state = Launch { launchModelPrime | currentPosition = (currentX + launchModelPrime.velocityX, currentY + launchModelPrime.velocityY)
                                                           , velocityY = (launchModelPrime.velocityY + gravity) * airResistance }
              }, Cmd.none)
      _ -> (model, Cmd.none)

advanceToNextLevel : Model -> LaunchModel -> Model
advanceToNextLevel model launchModel = { model | state = Aim (AimModel (launchModel.level + 1) (2, 58) launchModel.aimAngle) }

checkCollision : Int -> Point -> Bool
checkCollision level (floatX, floatY) =
  let
    x = round floatX
    y = round floatY
  in
    case Array.get level levels of
      Nothing -> True
      Just levelData ->
        if x < 0 then
          True
        else if x >= 95 then
          True
        else if y < 0 then
          True
        else if y >= 60 then
          True
        else
          let
            row = floor (toFloat y / 5)
            col = floor (toFloat x / 5)
          in
            case Array.get row levelData of
              Nothing -> True
              Just rowData ->
                case Array.get col rowData of
                  Nothing -> True
                  Just cell -> cell

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
    Launch launchModel -> drawLevel launchModel.level

drawLevel : Int -> List Canvas.Renderable
drawLevel levelNum = case Array.get levelNum levels of
   Nothing -> []
   Just level -> Array.foldl (++) [] (Array.indexedMap drawRow level)

drawRow : Int -> Array Bool -> List Canvas.Renderable
drawRow rowNum row = Array.foldl (++) [] (Array.indexedMap (drawCell rowNum) row)

drawCell : Int -> Int -> Bool -> List Canvas.Renderable
drawCell rowNum colNum cell =
  if cell then
    [shapes [ fill Color.black ] [ rect (toFloat colNum * 5, toFloat rowNum * 5) 5 5] ]
  else
    []

clearScreen : Canvas.Renderable
clearScreen = clear ( 0, 0 ) (toFloat width) (toFloat height)

foreground : Model -> List Canvas.Renderable
foreground model =
  case model.state of
     Menu menuSelection -> [ text [ gameFont ] (72, getYCoordinateFromMenuSelection menuSelection) "<" ]
     Aim aimModel -> drawMortle aimModel
     Launch launchModel -> [ shapes [ fill Color.black ] [ rect (roundPoint launchModel.currentPosition) 1 1 ] ]

getYCoordinateFromMenuSelection : MenuSelection -> Float
getYCoordinateFromMenuSelection menuSelection =
  case menuSelection of
    NewGame -> 24
    Continue -> 32

drawMortle : AimModel -> List Canvas.Renderable
drawMortle aimModel =
  let
    (posX, posY) = aimModel.currentPosition
    aimAngle = aimModel.aimAngle
    endX = toFloat (round (posX + aimLength * sin(aimAngle)))
    endY = toFloat (round (posY - aimLength * cos(aimAngle)))
  in
    shapes [ fill Color.black ] [ rect (posX - 1, posY - 1) 3 3 ] ::
    drawLine posX posY endX endY

roundPoint : Point -> Point
roundPoint (x, y) = (toFloat (round x), toFloat (round y))

drawLine : Float -> Float -> Float -> Float -> List Canvas.Renderable
drawLine x0 y0 x1 y1 =
  let
    dx = abs (x1 - x0)
    dy = abs (y1 - y0)
    sx = if x0 < x1 then 1.0 else -1.0
    sy = if y0 < y1 then 1.0 else -1.0
    err = dx - dy
  in
    drawLineCore x0 y0 x1 y1 dx dy sx sy err

drawLineCore : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> List Canvas.Renderable
drawLineCore x0 y0 x1 y1 dx dy sx sy err =
  invertPixel x0 y0 ::
  if (x0 == x1) && (y0 == y1)
    then
      []
    else
      let
        e2 = 2.0 * err
        (newX0, newErr) = if e2 > -dy
          then
            (x0+sx, err-dy)
          else
            (x0, err)
        (newY0, newNewErr) = if e2 < dx
          then
            (y0+sy, newErr+dx)
          else
            (y0, newErr)
      in
        drawLineCore newX0 newY0 x1 y1 dx dy sx sy newNewErr

invertPixel : Float -> Float -> Canvas.Renderable
invertPixel x y = shapes [ fill Color.black ] [ rect (x, y) 1 1 ]