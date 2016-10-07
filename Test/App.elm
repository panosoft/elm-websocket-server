port module Test.App exposing (..)

import Html exposing (..)
import Html.App
import Websocket exposing (..)


port node : Float -> Cmd msg


type alias Model =
    { wsPort : WSPort
    , path : String
    , receiveCount : Int
    , serverStarted : Bool
    , listenError : Bool
    }


type Msg
    = Nop
    | ServerError ( WSPort, String )
    | Server ( WSPort, ServerStatus )
    | ListenError ( WSPort, Path, String )
    | WSMessage ( ClientId, QueryString, String )
    | Connection ( WSPort, ClientId, ConnectionStatus )
    | SendError ( WSPort, ClientId, String )
    | Sent ( WSPort, ClientId, String )


initModel : Model
initModel =
    { wsPort = 8080
    , path = "/"
    , receiveCount = 0
    , serverStarted = False
    , listenError = False
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            initModel
    in
        model ! [ Websocket.startServer ServerError Server model.wsPort ]


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
                { model | serverStarted = True } ! []

        ListenError ( wsPort, path, error ) ->
            let
                l =
                    Debug.log "ListenError" ( wsPort, path, error )
            in
                { model | listenError = True } ! []

        WSMessage ( clientId, queryString, message ) ->
            let
                l =
                    Debug.log "WSMessage" ( clientId, queryString, message )
            in
                model ! [ Websocket.send model.wsPort model.path clientId message ]

        Connection ( wsPort, clientId, status ) ->
            let
                l =
                    Debug.log "Connection" ( wsPort, clientId, status )
            in
                model ! []

        SendError ( wsPort, clientId, error ) ->
            let
                l =
                    Debug.log "SendError" ( wsPort, clientId, error )
            in
                model ! []

        Sent ( wsPort, clientId, message ) ->
            let
                l =
                    Debug.log "Send" ( wsPort, clientId, message )
            in
                { model | receiveCount = model.receiveCount + 1 } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.serverStarted && (not model.listenError) && (model.receiveCount < 3) of
        True ->
            Websocket.listen ListenError SendError Sent WSMessage Connection model.wsPort model.path

        False ->
            Sub.none
