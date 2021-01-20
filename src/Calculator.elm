module Calculator exposing (Model,Msg,subscriptions,init,update,view,request,emptySelection)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Dom exposing (Viewport)
import Browser.Events exposing (onResize)
import Csv
import Html exposing (input, p, text)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onInput)
import Http
import Iso8601 exposing (fromTime, toTime)
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Coordinate as Coordinate
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import List.Extra exposing (find)
import String exposing (left)
import Svg
import Svg.Attributes
import Task
import Time exposing (Posix, millisToPosix, posixToMillis, utc)
import Pages.StaticHttp as StaticHttp
import Pages.Secrets as Secrets


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions -- always Sub.none
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ onResize SetScreenSize ]


-- MODEL

factor : Float
factor = 1.34 --ktCo2/day/Twh/year
              --
offset : Float
offset = 0.049 -- MtCo2
               --
type alias Model =
    { data : Data
    , compound : Data
    , totalBtc : Data
    , perBtcComp : Data
    , hovered : Maybe Datum
    , selection : Selection
    , dragging : Bool
    , hinted : Maybe Datum
    , startString : String
    , endString : String
    , btcS : String
    , width : Int
    , height : Int
    }


type alias Selection =
    { start : Maybe Datum
    , end : Maybe Datum
    } ---beware, start might be later than end!


type alias Data =
    List Datum


type alias Datum =
    { time : Posix
    , amount : Float
    }



-- INIT

init : ( Model, Cmd Msg )
init =
    ( { data = []
      , compound = []
      , totalBtc = []
      , perBtcComp = []
      , hovered = Nothing
      , selection = emptySelection
      , dragging = False
      , hinted = Nothing
      , startString = "YYYY-MM-DD"
      , endString = "YYYY-MM-DD"
      , btcS = ""
      , width = 1920
      , height =1080
      }
    , Task.perform (\vp -> let get = (\s -> s vp.viewport |> round) in
                               SetScreenSize (get .width) (get .height))
                                getViewport
    )

request : StaticHttp.Request
          { data : Data
          , compound : Data
          , totalBtc : Data
          , perBtcComp : Data
          , selection : Selection
          , startString : String
          , endString : String
          }
request = StaticHttp.map2
          (\nd tbtc ->
               let
                cp = mkcompound nd
                s = List.head cp
                e = List.head <| List.drop (List.length cp - 1) cp
                dTS = Maybe.withDefault "YYYY-MM-DD" << Maybe.map datumToTimeString
               in
                   { data = nd
                   , compound = cp
                   , selection =
                         { start = s
                         , end = e
                         }
                   , startString = dTS s
                   , endString = dTS e
                   , perBtcComp = mkPerBtcComp nd tbtc
                   , totalBtc = tbtc
                   }
          )
          (StaticHttp.unoptimizedRequest
              (Secrets.succeed
                   { url = "https://cbeci.org/api/csv"
                   , method = "GET"
                   , headers = []
                   , body = StaticHttp.emptyBody
                   }
              )
              (StaticHttp.expectString
                   (\content -> parseData content (parsePowRecord factor)))
          )
          (StaticHttp.unoptimizedRequest
               (Secrets.succeed
                    { url = "https://api.blockchain.info/charts/total-bitcoins?timespan=100years&format=csv&cors=true"
                    , method = "GET"
                    , headers = []
                    , body = StaticHttp.emptyBody
                    }
               )
               (StaticHttp.expectString
                    (\content -> parseData content parseBtcRecord))
          )

-- API

emptySelection : Selection
emptySelection =
    { start = Nothing, end = Nothing }


setSelection : Selection -> Model -> Model
setSelection selection model =
    { model | selection = selection }


setDragging : Bool -> Model -> Model
setDragging dragging model =
    { model | dragging = dragging }


setHovered : Maybe Datum -> Model -> Model
setHovered hovered model =
    { model | hovered = hovered }


setHint : Maybe Datum -> Model -> Model
setHint hinted model =
    { model | hinted = hinted }


getSelectionStart : Datum -> Model -> Datum
getSelectionStart hovered model =
    case model.selection.start of
        Just s ->
            s

        Nothing ->
            hovered


setSelectionString : Datum -> Datum -> Model -> Model
setSelectionString start end model =
    { model
        | startString = start |> datumToTimeString
        , endString = end |> datumToTimeString
    }



-- UPDATE


type Msg
    = SetScreenSize Int Int
        -- Chart 1
    | Hold Data
    | Move Data
    | Drop Data
    | LeaveChart Data
    | LeaveContainer Data
      -- Chart 2
    | Hint (Maybe Datum)
    | ChangeStart String
    | ChangeEnd String
    | ChangeBtc String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetScreenSize w h -> { model | height = h
                                     , width = w }
                             |> addCmd Cmd.none

        Hold point ->
            model
                |> setSelection emptySelection
                |> setDragging True
                |> addCmd Cmd.none

        Move (point :: xs) ->
            if model.dragging then
                let
                    start =
                        getSelectionStart point model

                    newSelection =
                        Selection (Just start) (Just point)
                in
                model
                    |> setSelection newSelection
                    |> setSelectionString start point
                    |> setHovered (Just point)
                    |> addCmd Cmd.none

            else
                model
                    |> setHovered (Just point)
                    |> addCmd Cmd.none

        Drop (point :: xs) ->
            if point == getSelectionStart point model then
                model
                    |> setSelection emptySelection
                    |> setDragging False
                    |> addCmd Cmd.none

            else
                model
                    |> setDragging False
                    |> addCmd Cmd.none

        LeaveChart point ->
            model
                |> setHovered Nothing
                |> addCmd Cmd.none

        LeaveContainer point ->
            model
                |> setDragging False
                |> setHovered Nothing
                |> addCmd Cmd.none

        Hint point ->
            model
                |> setHint point
                |> addCmd Cmd.none

        ChangeStart timeString ->
            addCmd Cmd.none <|
                let selection = model.selection
                in
                    { model
                        | startString = timeString
                        , selection =
                          case toTime timeString of
                              Ok ts ->
                                { selection | start = find (\d -> d.time == ts) model.compound }

                              Err _ ->
                                selection
                    }

        ChangeEnd timeString ->
            addCmd Cmd.none <|
                let selection = model.selection
                in
                    { model
                        | endString = timeString
                        , selection =
                          case toTime timeString of
                              Ok ts ->
                                { selection | end = find (\d -> d.time == ts) model.compound }

                              Err _ ->
                                selection
                    }

        ChangeBtc bS ->
            addCmd Cmd.none <|
                { model | btcS = bS }

        _ ->
            model |> addCmd Cmd.none


mkcompound : Data -> Data
mkcompound nd =
    let
        ( sum, summedList ) = List.foldl f ( 0, [] ) nd

        f d ( s, l ) = let ns = s + d.amount / 1000 in
                       ( ns, l ++ [ { d | amount = ns } ] )
    in summedList


mkPerBtcComp : Data -> Data -> Data
mkPerBtcComp nd amts =
    let
        ( sum, summedList ) =
            List.foldl f ( 0, [] ) nd

        f d ( s, l ) =
            let
                ns = s + d.amount / firstAmount amts d.time
            in
                ( ns, l ++ [ { d | amount = ns } ] )
    in summedList


firstAmount : Data -> Posix -> Float
firstAmount ls t =
    case find (\d -> fromTime d.time >= fromTime t) ls of
        Just d ->
            d.amount / 1000

        Nothing ->
            21000


addCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
addCmd cmd model =
    ( model, cmd )


parseData : String -> (List String -> Maybe Datum) -> Result String Data
parseData s f =
    case Csv.parse s of
        Ok c ->
            Ok (List.filterMap f c.records)

        Err _ ->
            Err "Parsing error"


parsePowRecord : Float -> List String -> Maybe Datum
parsePowRecord f l =
    case l of
        [ ts, dat, max, min, guess ] ->
            let
                time =
                    millisToPosix (1000 * Maybe.withDefault 0 (String.toInt ts))

                amount =
                    f * Maybe.withDefault 0 (String.toFloat guess)
            in
            -- if Time.toWeekday Time.utc time == Time.Mon then
            Just { time = time, amount = amount }

        -- else Nothing
        _ ->
            Nothing


parseBtcRecord : List String -> Maybe Datum
parseBtcRecord l =
    case l of
        [ ts, am ] ->
            case ( toTime (left 10 ts), String.toFloat am ) of
                ( Ok t, Just f ) ->
                    Just { time = t, amount = f }

                _ ->
                    Nothing

        _ ->
            Nothing



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div [ Html.Attributes.style "display" "flex" ] <|
        [ p [ style "white-space" "pre" ] <|
            let
                s = model.startString
                e = model.endString
            in
                (case ( model.selection.start, model.selection.end ) of

                     ( Just startDatum, Just endDatum ) ->
                        (let
                            total = abs(endDatum.amount - startDatum.amount)
                         in
                            [ chart1 model
                            , chart2 model
                            , text ("The horizontal line at " ++ String.fromFloat offset ++ " Mt signifies the amount of Co2 that has already been offset today.")
                            , text ("\nCalculated from the Genesis block at January 3, 2009, this means we've offset Bitcoin's history approximately until " ++ findOffsetDate model ++ ".")
                            , text "\nPlease select or enter time interval: "
                            , input [ placeholder s, value s, onInput ChangeStart ] []
                            , input [ placeholder e, value e, onInput ChangeEnd ] []

                            --   , chartZoom model startDatum endDatum
                            , text ("\nSelected: " ++ datumToTimeString startDatum)
                            , text (" to " ++ datumToTimeString endDatum)
                            , text
                                ("\nTotal Co2 in this time frame: "
                                    ++ (String.fromFloat <| round100 <| total)
                                    ++ " Mt"
                                )
                            ]
                            ++ let perBtcAmount = co2perBtcIn model.perBtcComp startDatum endDatum in
                               [ text
                                     ("\nPer bitcoin when divided by the total amount in existence at every day in the interval: "
                                          ++ (String.fromFloat <| round100 <| perBtcAmount)
                                     )
                               ]
                            ++ [ text " t." ]
                            ++ [ text "\nHow much Bitcoin do you want to offset? "
                               , input [ placeholder "0.00000001", value model.btcS, onInput ChangeBtc ] []
                               ]
                            ++ case String.toFloat model.btcS of
                                   Nothing ->
                                        []

                                   Just btc ->
                                        [ text ("\nThis is equivalent to "
                                            ++ (String.fromFloat <| round100 <| perBtcAmount * btc)
                                            ++ " t Co2.\nHappy offsetting, and don't forget to tell us about it so we can keep count!"
                                               )
                                        ])

                     _ ->
                            [ chart1 model
                            , chart2 model
                            , text "Please select or enter a time interval: "
                            , input [ placeholder "start date", value s, onInput ChangeStart ] []
                            , input [ placeholder "end date", value e, onInput ChangeEnd ] []
                            ]
                     )
                ]



-- MAIN CHARTS


chart1 : Model -> Html.Html Msg
chart1 model =
    LineChart.viewCustom
        (chartConfig
             { y = yAxis1 (model.height)
             , area = Area.normal 0.5
             , range = Range.default
             , junk =
                 Junk.hoverOne model.hinted
                     [ ( "date", datumToTimeString )
                     , ( "kt/d", String.fromFloat << round100 << .amount )
                     ]
             , events = Events.hoverOne Hint
             , legends = Legends.default
             , dots = Dots.custom (Dots.full 0)
             , id = "line-chart"
             , width = model.width
             }
        )
    [ LineChart.line Colors.pink Dots.circle "CO2" model.data ]


chart2 : Model -> Html.Html Msg
chart2 model =
    LineChart.viewCustom
        (chartConfig
             { y = yAxis2 (model.height)
             , area = Area.default
             , range = Range.default
             , junk = junkConfig model
             , legends = Legends.default
             , events =
                 Events.custom
                     [ Events.onWithOptions "mousedown" (Events.Options True True False) Hold Events.getNearestX
                     , Events.onWithOptions "mousemove" (Events.Options True True False) Move Events.getNearestX
                     , Events.onWithOptions "mouseup" (Events.Options True True True) Drop Events.getNearestX
                     , Events.onWithOptions "mouseleave" (Events.Options True True False) LeaveChart Events.getNearestX
                     , Events.onWithOptions "mouseleave" (Events.Options True True True) LeaveContainer Events.getNearestX
                     ]
             , dots = Dots.custom (Dots.full 0)
             , id = "line-chart"
             , width = model.width
             }
        )
    [ LineChart.line Colors.blue Dots.circle "CO2 total" model.compound ]


junkConfig : Model -> Junk.Config Datum msg
junkConfig model =
    Junk.custom <|
        \system ->
            { below = below system model.selection
            , above = above system model.hovered offset
            , html = []
            }


below : Coordinate.System -> Selection -> List (Svg.Svg msg)
below system selection =
    case ( selection.start, selection.end ) of
        ( Just startDatum, Just endDatum ) ->
            [ Junk.rectangle system
                [ Svg.Attributes.fill "#b6b6b61a" ]
                (datumToFloat startDatum)
                (datumToFloat endDatum)
                system.y.min
                system.y.max
            ]

        _ ->
            []


above : Coordinate.System -> Maybe Datum -> Float -> List (Svg.Svg msg)
above system maybeHovered o =
    Junk.horizontal system [] o
        :: (case maybeHovered of
                Just hovered ->
                    [ Junk.vertical system [] (datumToFloat hovered) ]

                Nothing ->
                    []
           )



-- VIEW CHART


type alias Config =
    { y : Axis.Config Datum Msg
    , range : Range.Config
    , junk : Junk.Config Datum Msg
    , events : Events.Config Datum Msg
    , legends : Legends.Config Datum Msg
    , dots : Dots.Config Datum
    , id : String
    , area : Area.Config
    , width : Int
    }


chartConfig : Config -> LineChart.Config Datum Msg
chartConfig { y, range, junk, events, legends, dots, id, area, width } =
    { y = y
    , x = xAxis range width
    , container =
        Container.custom
            { attributesHtml = [ Html.Attributes.style "font-family" "monospace" ]
            , attributesSvg = []
            , size = Container.static
            , margin = Container.Margin 30 200 60 50
            , id = id
            }
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = legends
    , events = events
    , junk = junk
    , grid = Grid.default
    , area = area
    , line = Line.default
    , dots = dots
    }


yAxis1 : Int -> Axis.Config Datum Msg
yAxis1 h =
    Axis.full (h//2) "kt/day" .amount


yAxis2 : Int -> Axis.Config Datum Msg
yAxis2 h =
    Axis.full (h//2) "Mt" .amount


xAxis : Range.Config -> Int -> Axis.Config Datum Msg
xAxis range w =
    Axis.custom
        { title = Title.default "date"
        , variable = Just << datumToFloat
        , pixels = w
        , range = range
        , axisLine = AxisLine.full Colors.gray
        , ticks = Ticks.time utc 7
        }



-- UTILS


datumToFloat : Datum -> Float
datumToFloat =
    toFloat << posixToMillis << .time


datumToTimeString : Datum -> String
datumToTimeString =
    String.left 10 << fromTime << .time


round100 : Float -> Float
round100 float =
    toFloat (round (float * 100)) / 100


findOffsetDate : Model -> String
findOffsetDate model =
    let
        fOD s l =
            case l of
                [] ->
                    s

                x :: xs ->
                    if x.amount < offset then
                        fOD (datumToTimeString x) xs

                    else
                        s
    in fOD "2009-03-01" model.data


co2perBtcIn : Data -> Datum -> Datum -> Float
co2perBtcIn l startDatum endDatum =
    let
        findAmount x =
            case find (\d -> fromTime d.time >= fromTime x.time) l of
                Nothing ->
                    0

                Just d ->
                    d.amount
    in
    abs(findAmount endDatum - findAmount startDatum)
