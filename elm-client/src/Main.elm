module Main exposing (main)

import Browser
import Http
import Html exposing (Html, Attribute, div, text, table, tr, td, th, node, h2)
import Html.Attributes exposing (href, rel, class)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)
import Json.Decode as De
import Json.Encode as En
import Dict exposing (Dict)
--https://elmprogramming.com/creating-a-new-post.html

serverUrl : String
serverUrl = "http://localhost:3000"

main = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions}

type alias Name = String
type alias TableHeading = String
type alias TableRow = Dict TableHeading String
type Table = Table Name (List TableHeading) (List TableRow)
type Model = Model Table

type Msg = GotResp (Result Http.Error Table)

init : () -> (Model, Cmd Msg)
init _ = 
    let
        m = Model (Table "" [] [])
        c = getTable
    in  (m, c)

update : Msg -> Model -> (Model, Cmd Msg)
update msg (Model table) =
    case msg of
        GotResp (Ok (t1)) ->
            (Model t1, Cmd.none)
        GotResp (Err error) ->
            let dummy = Debug.log "FAILED" error
            in (Model table, Cmd.none)

view : Model -> Html Msg
view (Model (Table name headings rows)) =
        div [] [
            stylesheet,
            template [
                h2 [] [text name],
                table [] ((renderTh headings)::(renderRows headings rows))
            ]
        ]

template : List (Html Msg) -> Html Msg
template xs = div [class "row"] [div [class "col s8 offset-s2"] xs]

renderTh : List TableHeading -> Html Msg
renderTh = tr [] << (List.map (\x -> th [] <| [text x]))

renderRows : List TableHeading -> List TableRow -> List(Html Msg)
renderRows hs = List.map (\xs -> tr [] (
    List.map (\k -> 
            td [] [text <| withDefault "-" (Dict.get k xs)]
        ) hs ))

stylesheet : Html Msg
stylesheet = node "link" [rel "stylesheet", href "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css"] []

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

getTable : Cmd Msg
getTable =
    Http.get
        { url = serverUrl ++ "/t/dummy"
        , expect = Http.expectJson GotResp decTable
        }

decTableHeading : De.Decoder TableHeading
decTableHeading = De.string

decTableRow : De.Decoder TableRow
decTableRow = De.dict <| De.string


decTable : De.Decoder Table
decTable =
    De.map3 Table
        (De.field "table_name" De.string)
        (De.field "table_headings" (De.list <| decTableHeading))
        (De.field "table_rows" (De.list <| decTableRow))
