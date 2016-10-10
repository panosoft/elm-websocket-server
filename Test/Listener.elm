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
    | Connection ( WSPort, ClientId, ConnectionStatus )
    | ListenError ( WSPort, Path, String )
    | WSMessage ( ClientId, QueryString, String )
    | SendError ( WSPort, ClientId, String )
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

        Connection ( wsPort, clientId, status ) ->
            let
                l =
                    Debug.log "Connection" ( wsPort, clientId, status )
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
                model ! [ Websocket.send model.wsPort model.path clientId message ]

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
    case not model.listenError && model.receiveCount < 3 of
        True ->
            Websocket.listen ListenError SendError Sent WSMessage Connection model.wsPort model.path

        False ->
            Sub.none
