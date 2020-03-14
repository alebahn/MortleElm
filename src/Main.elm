port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown, onAnimationFrame)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Lazy exposing (lazy)
import Canvas exposing (clear, shapes, rect, text, Point)
import Canvas.Settings exposing (Setting, fill, stroke)
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

main : Program Flags Model Msg
main =
  Browser.element
  { init = init
  , subscriptions = subscriptions
  , update = update, view = view }

-- PORTS

port saveLevel : Int -> Cmd msg

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
    Menu _ -> onKeyDown (Decode.map keyToMenuMsg (Decode.field "key" Decode.string))
    Aim _ -> onKeyDown (Decode.map keyToAimMsg (Decode.field "key" Decode.string))
    Launch _ -> onAnimationFrame (\_ -> Frame)
    Win -> onKeyDown (Decode.succeed WinContinue)

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
type alias Flags = Int
type alias Model = { state : GameState
                   , startLevel : Int
                   }
type GameState = Menu MenuSelection
               | Aim AimModel
               | Launch LaunchModel
               | Win
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


init : Flags -> (Model, Cmd Msg)
init continueLevel =
  (Model (Menu NewGame) continueLevel, Cmd.none)

-- UPDATE

type Msg
  = MenuDown
  | MenuUp
  | MenuAccept
  | AimLeft
  | AimRight
  | AimLaunch
  | KeyOther
  | WinContinue
  | Frame

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model.state of
    Menu menuSelection -> case msg of
       MenuUp -> ({ model | state = Menu NewGame }, Cmd.none)
       MenuDown -> ({ model | state = Menu Continue }, Cmd.none)
       MenuAccept -> ({ model | state = Aim (AimModel (getStartLevel model.startLevel menuSelection) (2, 58) 0) }, Cmd.none)
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
            advanceToNextLevel model launchModel
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
    Win -> case msg of
      WinContinue -> (Model (Menu NewGame) (Array.length levels - 1), Cmd.none)
      _ -> (model, Cmd.none)

getStartLevel : Int -> MenuSelection -> Int
getStartLevel continueLevel menuSelection =
  case menuSelection of
    NewGame -> 0
    Continue -> continueLevel

advanceToNextLevel : Model -> LaunchModel -> (Model, Cmd Msg)
advanceToNextLevel model launchModel =
  if launchModel.level + 1 >= Array.length levels then
    ({ model | state = Win }, Cmd.none)
  else
    ({ model | state = Aim (AimModel (launchModel.level + 1) (2, 58) 0) }, saveLevel (launchModel.level + 1))

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
  Html.div [ class "gameContainer" ]
  [ lazy backgroundCanvas (modelToBackgroundModel model)
  , Canvas.toHtml
      ( width, height )
      [ class "foreground"]
      (foreground model)
  ]

gameFont : Setting
gameFont = font {size = 8, family = "'Press Start 2P'"}

type alias BackgroundModel = Int

menuScreen : BackgroundModel
menuScreen = -1
levelScreen : Int -> BackgroundModel
levelScreen level = level
winScreen : BackgroundModel
winScreen = -2

modelToBackgroundModel : Model -> BackgroundModel
modelToBackgroundModel model =
  case model.state of
    Menu _ -> menuScreen
    Aim aimModel -> levelScreen aimModel.level
    Launch launchModel -> levelScreen launchModel.level
    Win -> winScreen

backgroundCanvas : BackgroundModel -> Html Msg
backgroundCanvas model =
  Canvas.toHtml
        ( width, height )
        [ class "background"]
        (background model)

background : BackgroundModel -> List Canvas.Renderable
background model =
  clearScreen ::
  if model == menuScreen then
    [ text [ gameFont ] (24, 12) "MORTLE"
    , text [ gameFont ] (4, 24) "New Game"
    , text [ gameFont ] (4, 32) "Continue"
    ]
  else if model == winScreen then
    [ text [ gameFont] (16, 12) "You Win!"
    , text [ gameFont] ( 4, 24) "Press any"
    , text [ gameFont] ( 4, 32) "key to"
    , text [ gameFont] ( 4, 40) "return to"
    , text [ gameFont] ( 4, 48) "menu"
    ]
  else
    drawLevel model

drawLevel : Int -> List Canvas.Renderable
drawLevel levelNum = case Array.get levelNum levels of
  Nothing -> []
  Just level -> Array.foldl (++) [] (Array.indexedMap drawRow level) ++ drawDoor

drawRow : Int -> Array Bool -> List Canvas.Renderable
drawRow rowNum row = Array.foldl (++) [] (Array.indexedMap (drawCell rowNum) row)

drawCell : Int -> Int -> Bool -> List Canvas.Renderable
drawCell rowNum colNum cell =
  if cell then
    [shapes [ fill Color.black ] [ rect (toFloat colNum * 5, toFloat rowNum * 5) 5 5] ]
  else
    []

drawDoor : List Canvas.Renderable
drawDoor =
  [ shapes [ stroke Color.black ] [ rect (90.5, 55.5) 4 4 ]
  , shapes [ fill Color.black] [ rect (93, 57) 1 1 ]
  ]

clearScreen : Canvas.Renderable
clearScreen = clear ( 0, 0 ) (toFloat width) (toFloat height)

foreground : Model -> List Canvas.Renderable
foreground model =
  clearScreen ::
  case model.state of
     Menu menuSelection -> [ text [ gameFont ] (72, getYCoordinateFromMenuSelection menuSelection) "<" ]
     Aim aimModel -> drawMortle aimModel
     Launch launchModel -> [ shapes [ fill Color.black ] [ rect (roundPoint launchModel.currentPosition) 1 1 ] ]
     Win -> []

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
    drawLine aimModel.level posX posY endX endY

roundPoint : Point -> Point
roundPoint (x, y) = (toFloat (round x), toFloat (round y))

drawLine : Int -> Float -> Float -> Float -> Float -> List Canvas.Renderable
drawLine level x0 y0 x1 y1 =
  let
    dx = abs (x1 - x0)
    dy = abs (y1 - y0)
    sx = if x0 < x1 then 1.0 else -1.0
    sy = if y0 < y1 then 1.0 else -1.0
    err = dx - dy
  in
    drawLineCore level x0 y0 x1 y1 dx dy sx sy err

drawLineCore : Int -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> List Canvas.Renderable
drawLineCore level x0 y0 x1 y1 dx dy sx sy err =
  invertPixel level (x0, y0) ::
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
        drawLineCore level newX0 newY0 x1 y1 dx dy sx sy newNewErr

invertPixel : Int -> Point -> Canvas.Renderable
invertPixel level point =
  let
    fillColor =
      if checkCollision level point then
        Color.white
      else
        Color.black
  in
    shapes [ fill fillColor ] [ rect point 1 1 ]