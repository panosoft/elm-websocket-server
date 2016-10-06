port module Test.App exposing (..)

import Html exposing (..)
import Html.App
import Websocket exposing (..)


port node : Float -> Cmd msg


type alias Model =
    { wsPort : Maybe WSPort
    , listenError : Bool
    }


type Msg
    = Nop
    | ServerError ( WSPort, String )
    | Server ( WSPort, ServerStatus )
    | ListenError ( WSPort, Path, String )
    | WSMessage String
    | Connection ( WSPort, ClientId, ConnectionStatus )


initModel : Model
initModel =
    { wsPort = Nothing
    , listenError = False
    }


init : ( Model, Cmd Msg )
init =
    initModel ! [ Websocket.startServer ServerError Server 8080 ]


main : Program Never
main =
    -- N.B. the dummy init which returns an empty Model and no Cmd
    -- N.B. the dummy view returns an empty HTML text node
    --      this is just to make the compiler happy since the worker() call Javascript doesn't use a render
    Html.App.program
        { init = init
        , view = (\_ -> text "")
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            model ! []

        ServerError ( wsPort, error ) ->
            let
                l =
                    Debug.log "ServerError" ( wsPort, error )
            in
                model ! []

        Server ( wsPort, status ) ->
            let
                l =
                    Debug.log "Server" ( wsPort, status )
            in
                { model | wsPort = Just wsPort } ! []

        ListenError ( wsPort, path, error ) ->
            let
                l =
                    Debug.log "ListenError" ( wsPort, path, error )
            in
                { model | listenError = True } ! []

        WSMessage message ->
            let
                l =
                    Debug.log "WSMessage" message
            in
                model ! []

        Connection ( wsPort, clientId, status ) ->
            let
                l =
                    Debug.log "Connection" ( wsPort, clientId, status )
            in
                model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.listenError of
        True ->
            Sub.none

        False ->
            Maybe.withDefault Sub.none <|
                (model.wsPort
                    |> Maybe.map (\wsPort -> Websocket.listen ListenError WSMessage Connection wsPort "")
                )
