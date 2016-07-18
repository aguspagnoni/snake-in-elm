import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Keyboard
import Time exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Text
import Char
import Random exposing (..)
import AnimationFrame
import String exposing (slice, toUpper)

segmentDim = 15
borderSize = segmentDim
(width, height) = (800, 600)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- step 1: define your Model
type Direction
  = Up
  | Down
  | Left
  | Right

type Difficulty
  = Easy
  | Medium
  | Hard

type alias Position = (Float, Float)

pos : Float -> Float -> Position
pos = (,)

type alias Player =
  { head: Position
  , tail: List Position
  , direction: Direction
  , name: String }

type Model
  = NotStarted String
  | Started Difficulty (Player, Player)

-- step 2: define Msg that can trigger updates to Model
type Msg
  = Tick Time
  | KeyPress Keyboard.KeyCode

-- step 3: define the initial state for the app
initPlayer : String -> Position -> Player
initPlayer name iniPos =
  let head = iniPos
  in { head=head, tail=[], direction=Down, name=name }

init : (Model, Cmd Msg)
init = (NotStarted "", Cmd.none)

-- step 4: define your subscriptions - WebSocket, Keyboard, etc.
subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    NotStarted _ ->
      Keyboard.presses KeyPress

    Started difficulty (_,_) ->
      Sub.batch
        [ Keyboard.presses KeyPress
        , Time.every (Time.inMilliseconds (speedFor difficulty)) Tick --Time.every (Time.inMilliseconds 50) Tick
        ]

speedFor : Difficulty -> Float
speedFor difficulty =
  case difficulty of
    Easy -> 100.0
    Medium -> 50.0
    Hard -> 30.0

-- step 5: how to render your model
view : Model -> Html Msg
view model =
  let bg = rect (toFloat width) (toFloat height) |> filled fieldColor
      border = rect (toFloat width) (toFloat height) |> outlined { defaultLine | width = borderSize, color = black}
      content =
        case model of
          NotStarted winner  ->
            let
              announceWinner = if winner == "" then "" else "The winner is: " ++ winner
              wholeText = announceWinner ++ "\npress 1,2 or 3 to start with different difficulties" ++ "\n\n Controls: \n P1: a,w,d,s\n P2: j,i,l,k"
            in [txt wholeText textColor]

          Started _ (p1, p2) ->
            let
              p1head = rect segmentDim segmentDim |> filled green |> move p1.head
              p1tail = p1.tail |> List.map (\pos -> rect segmentDim segmentDim |> filled green |> move pos)
              p1char = txt (shortName p1.name) white |> move p1.head

              p2head = rect segmentDim segmentDim |> filled red |> move p2.head
              p2tail = p2.tail |> List.map (\pos -> rect segmentDim segmentDim |> filled red |> move pos)
              p2char = txt (shortName p2.name) white |> move p2.head
            in [p1head, p1char]++p1tail++[p2head, p2char]++p2tail
  in collage width height (bg::border::content)
     |> Element.toHtml

-- step 6: implement state transition
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    NotStarted _->
      case msg of
        KeyPress keyCode ->
          case Char.fromCode keyCode of
            '1' -> startGameWithDifficulty Easy
            '2' -> startGameWithDifficulty Medium
            '3' -> startGameWithDifficulty Hard
            _   -> (model, Cmd.none)
        _ ->
          (model, Cmd.none)

    Started difficulty (p1,p2) ->
      case msg of
        KeyPress keyCode ->
          let (newDir, player) = getNewDirection keyCode (p1,p2)
              newPlayer = { player | direction=newDir }
              p1New = if player.head == p1.head then newPlayer else p1
              p2New = if player.head == p2.head then newPlayer else p2
          in (Started difficulty (p1New, p2New), Cmd.none)
        Tick _ ->
          let newHead1 = getNewSegment p1.head p1.direction
              newHead2 = getNewSegment p2.head p2.direction
              newTail1 = p1.head::p1.tail
              newTail2 = p2.head::p2.tail
              p1' = { p1 | head=newHead1, tail=newTail1 }
              p2' = { p2 | head=newHead2, tail=newTail2 }
              gameOver1 = isGameOver p1' p2'
              gameOver2 = isGameOver p2' p1'
          in if gameOver1 then
              (NotStarted p2'.name, Cmd.none)
             else if gameOver2 then
              (NotStarted p1'.name, Cmd.none)
             else
              (Started difficulty (p1', p2'), Cmd.none)

startGameWithDifficulty : Difficulty -> (Model, Cmd Msg)
startGameWithDifficulty difficulty =
  (Started difficulty
    (
      (initPlayer "1st player" (pos (toFloat 4*segmentDim) 0)),
      (initPlayer "2nd player" (pos (toFloat -4*segmentDim) 0))),
      Cmd.none
    )

txt : String -> Color -> Form
txt msg color =
  msg
  |> Text.fromString
  |> Text.color color
  |> Text.monospace
  |> Element.centered
  |> Collage.toForm

fieldColor = rgb 234 247 196
borderColor = rgb 88 80 79
textColor = rgb 90 120 127

getNewDirection : Char.KeyCode -> (Player, Player) -> (Direction, Player)
getNewDirection keyCode (p1, p2) =
  let (changeableDirs, newDir, playerTriggered) =
    case Char.fromCode keyCode of
      --player 1
      'a' -> ([ Up, Down ], Left, p1)
      'w' -> ([ Left, Right ], Up, p1)
      'd' -> ([ Up, Down ], Right, p1)
      's' -> ([ Left, Right ], Down, p1)
      --player 2
      'j' -> ([ Up, Down ], Left, p2)
      'i' -> ([ Left, Right ], Up, p2)
      'l' -> ([ Up, Down ], Right, p2)
      'k' -> ([ Left, Right ], Down, p2)
      _  -> ([], p1.direction, p1)
  in
    if List.any ((==) playerTriggered.direction) changeableDirs
    then (newDir, playerTriggered)
    else (playerTriggered.direction, playerTriggered)

getNewSegment : Position -> Direction -> Position
getNewSegment (x, y) direction =
  case direction of
    Up    -> pos x (y+segmentDim)
    Down  -> pos x (y-segmentDim)
    Left  -> pos (x-segmentDim) y
    Right -> pos (x+segmentDim) y

isGameOver : Player -> Player -> Bool
isGameOver p1 p2 =
  List.any ((==) p1.head) (p1.tail++[p2.head]++p2.tail)   -- eat itself or other player
  || fst p1.head > (width / 2)              -- hit right
  || snd p1.head > (-borderSize + height / 2)             -- hit top
  || fst p1.head < (-width / 2)                 -- hit left
  || snd p1.head < (borderSize-height / 2)                -- hit bottom

shortName : String -> String
shortName s =
  s |> slice 0 1 |> toUpper
