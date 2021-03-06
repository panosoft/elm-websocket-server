module Listener exposing (..)

import Websocket exposing (..)


type alias Model =
    { wsPort : WSPort
    , path : Path
    , receiveCount : Int
    , listenError : Bool
    }


type Msg
    = Nop
    | ConnectDisconnect ( WSPort, Path, ClientId, IPAddress, ConnectionStatus )
    | ListenError ( WSPort, Path, String )
    | WSMessage ( ClientId, QueryString, String )
    | SendError ( WSPort, ClientId, String, String )
    | Sent ( WSPort, ClientId, String )


initModel : WSPort -> Model
initModel wsPort =
    { wsPort = wsPort
    , path = "/"
    , receiveCount = 0
    , listenError = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            model ! []

        ConnectDisconnect ( wsPort, path, clientId, iPAddress, status ) ->
            let
                l =
                    Debug.log "ConnectionStatus" ( wsPort, path, clientId, iPAddress, status )
            in
                model ! []

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
                model ! [ Websocket.send SendError Sent model.wsPort clientId message ]

        SendError ( wsPort, clientId, message, error ) ->
            let
                l =
                    Debug.log "SendError" ( wsPort, clientId, message, error )
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
    -- case not model.listenError && model.receiveCount < 3 of
    case not model.listenError of
        True ->
            Websocket.listen ListenError WSMessage ConnectDisconnect model.wsPort model.path

        False ->
            Sub.none
