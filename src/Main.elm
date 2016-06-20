module Main exposing (..)

import Html exposing (Html, div, text)
import Html.App
import Html.Attributes exposing (style)
import Models as Models exposing (Model)
import TileLayer.View
import TileLayer.Messages


-- MODEL


init : ( Model, Cmd Msg )
init =
    ( Models.new 512 256, Cmd.none )



-- MESSAGES


type Msg
    = TileLayerMsg TileLayer.Messages.Msg



-- VIEW


view : Model -> Html Msg
view model =
    Html.App.map TileLayerMsg
        (div
            [ style
                [ ( "width", (toString model.size.x) ++ "px" )
                , ( "height", (toString model.size.y) ++ "px" )
                , ( "background", "wheat" )
                ]
            ]
            (List.map (TileLayer.View.view model.url) model.layers)
        )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TileLayerMsg _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
