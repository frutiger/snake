import Array
import Html
import Html.Attributes
import Random
import Time
import Tuple

import Ports

width  = 24
height = 16

offset position = position.y * width + position.x
position index = {
        x = (rem index width),
        y = (index // width)
    }

type Cell           = Empty | Food | Snake
type alias Grid     = Array.Array Cell
type Direction      = Left | Up | Right | Down
type alias Position = { x: Int, y: Int }

type alias Model = {
    alive:   Bool,
    head:    Position,
    history: List Direction,
    heading: Direction,
    food:    Position
}

type Msg
    = Tick      Time.Time
    | Keydown   String
    | PlaceFood Int

nextPosition : Direction -> Position -> Position
nextPosition direction position =
    let
        x = position.x
        y = position.y
    in
        case direction of
            Left  -> { x = x - 1, y = y     }
            Up    -> { x = x,     y = y - 1 }
            Right -> { x = x + 1, y = y     }
            Down  -> { x = x,     y = y + 1 }

prevPosition : Direction -> Position -> Position
prevPosition direction position =
    let
        x = position.x
        y = position.y
    in
        case direction of
            Left  -> { x = x + 1, y = y     }
            Up    -> { x = x,     y = y + 1 }
            Right -> { x = x - 1, y = y     }
            Down  -> { x = x,     y = y - 1 }

applyBackwardOnce : (Position -> a -> a) -> Direction -> (Position, a) -> (Position, a)
applyBackwardOnce func direction state =
    let
        head = prevPosition direction (Tuple.first state)
    in
        (head, func head (Tuple.second state))

applyBackward : (Position -> a -> a) -> a -> Position -> List Direction -> a
applyBackward func initial head history =
    Tuple.second (List.foldl (applyBackwardOnce func) (head, initial) history)

buildGrid : Position -> Grid -> Grid
buildGrid head grid =
    Array.set (offset head) Snake grid

toGrid : Position -> List Direction -> Grid
toGrid head history =
    applyBackward buildGrid (Array.repeat (width * height) Empty) head history

grow : Position -> List Position -> List Position
grow position body =
    List.append body [position]

toBody : Position -> List Direction -> List Position
toBody head history =
    applyBackward grow [] head history

init : (Model, Cmd Msg)
init = (let
        start   = { x =  8, y = 4 }
        history = List.repeat 5 Right
    in
        { alive   = True,
          head    = start,
          history = history,
          heading = Right,
          food    = { x = 16, y = 4 }
        }, Cmd.none)

applyTick : Model -> (Model, Bool)
applyTick model =
    let
        x       = model.head.x
        y       = model.head.y
        next    = nextPosition model.heading model.head
        body    = toBody model.head model.history
        selfEat = List.any ((==) model.head) body
        alive   = (x >= 0) && (y >= 0) && (x < width) && (y < height) && not selfEat
        didEat  = model.head == model.food
    in
        let
            history = if
                    alive
                then
                    List.take ((List.length model.history) + if didEat then 1 else 0) (model.heading :: model.history)
                else
                    model.history
            body    = toBody model.head history
        in
            if
                alive
            then
                ({ model |
                    alive   = True,
                    head    = next,
                    history = history
                }, didEat)
            else
                ({ model |
                    alive   = False,
                    head    = model.head,
                    history = history
                }, False)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick    time      -> let
                nextMove = applyTick model
            in
                (Tuple.first nextMove, if Tuple.second nextMove then Random.generate PlaceFood (Random.int 1 (width * height - 1)) else Cmd.none)
        Keydown direction -> (if
                model.alive
            then
                { model |
                    heading = case model.heading of
                        Up    -> case direction of
                            "left"  -> Left
                            "right" -> Right
                            _       -> model.heading
                        Down  -> case direction of
                            "left"  -> Left
                            "right" -> Right
                            _       -> model.heading
                        Left  -> case direction of
                            "up"   -> Up
                            "down" -> Down
                            _      -> model.heading
                        Right -> case direction of
                            "up"   -> Up
                            "down" -> Down
                            _      -> model.heading
                }
            else
                model,
            Cmd.none)
        PlaceFood index -> ({ model |
            food = position index
        }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        Time.every (100 * Time.millisecond) Tick,
        Ports.keydown Keydown
    ]

attributes : Int -> Cell -> List (Html.Attribute Msg)
attributes index cell =
    List.map Html.Attributes.class [
        "row" ++ (toString ((index // width)  + 1)),
        "col" ++ (toString ((rem index width) + 1)),
        toString cell
    ]

toTable : Grid -> Html.Html Msg
toTable grid =
    Html.div [Html.Attributes.class "grid"] (Array.toList (Array.indexedMap (\index cell -> Html.div (attributes index cell) []) grid))

view : Model -> Html.Html Msg
view model =
    toTable (Array.set (offset model.food) Food (toGrid model.head model.history))

main = Html.program { init          = init,
                      update        = update,
                      subscriptions = subscriptions,
                      view          = view           }

