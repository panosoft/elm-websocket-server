effect module Websocket
    where { command = MyCmd, subscription = MySub }
    exposing
        ( startServer
        , send
        , stopServer
        , listen
        , ServerStatus
        , ConnectionStatus
        , WSPort
        , Path
        , QueryString
        , ClientId
        )

{-| Websocket Server Effects Manager

The native driver is https://github.com/websockets/ws

# Commands
@docs startServer, send, stopServer, ServerStatus, ConnectionStatus, WSPort, Path, QueryString, ClientId

# Subscriptions
@docs listen
-}

import Dict exposing (Dict)
import Task exposing (Task)
import DebugF exposing (log, toStringF)
import Native.Websocket


-- API


type MyCmd msg
    = StartServer (ServerErrorTagger msg) (ServerTagger msg) WSPort
    | Send WSPort Path ClientId String
    | StopServer WSPort


type MySub msg
    = Listen (ListenErrorTagger msg) (SendErrorTagger msg) (SendTagger msg) (MessageTagger msg) (ConnectionTagger msg) WSPort Path



-- Types


{-| Native structure (opaque type)
-}
type WebsocketServer
    = WebsocketServer


{-| Native structure (opaque type)
-}
type Websocket
    = Websocket


{-| Websocket Port type
-}
type alias WSPort =
    Int


{-| Client Id
-}
type alias ClientId =
    Int


{-| Websocket Path type
-}
type alias Path =
    String


{-| Websocket QueryString type
-}
type alias QueryString =
    String


{-| Server Status types
-}
type ServerStatus
    = Started
    | Stopped



-- Taggers


type alias ServerErrorTagger msg =
    ( WSPort, String ) -> msg


type alias ListenErrorTagger msg =
    ( WSPort, Path, String ) -> msg


type alias SendErrorTagger msg =
    ( WSPort, ClientId, String ) -> msg


type alias SendTagger msg =
    ( WSPort, ClientId, String ) -> msg


type alias ServerTagger msg =
    ( WSPort, ServerStatus ) -> msg


type alias ConnectionTagger msg =
    ( WSPort, ClientId, ConnectionStatus ) -> msg


type alias MessageTagger msg =
    ( ClientId, QueryString, String ) -> msg


{-| Connection status
-}
type ConnectionStatus
    = Connected
    | Disconnected



-- State


type alias ClientDict =
    Dict ClientId Websocket


type alias Server msg =
    { wsServer : Maybe WebsocketServer
    , errorTagger : ServerErrorTagger msg
    , tagger : ServerTagger msg
    , clients : ClientDict
    }


type alias ServerDict msg =
    Dict WSPort (Server msg)


type alias ListenerTaggers msg =
    { sendErrorTagger : SendErrorTagger msg
    , sendTagger : SendTagger msg
    , messageTagger : MessageTagger msg
    , connectionTagger : ConnectionTagger msg
    }


type alias ListenerDict msg =
    Dict ( WSPort, Path ) (ListenerTaggers msg)


{-| Effects manager state
-}
type alias State msg =
    { servers : ServerDict msg
    , listeners : ListenerDict msg
    }


(//) : Maybe a -> a -> a
(//) =
    flip Maybe.withDefault


{-| lazy version of // operator
-}
(/!/) : Maybe a -> (() -> a) -> a
(/!/) maybe lazy =
    case maybe of
        Just x ->
            x

        Nothing ->
            lazy ()


(&>) : Task x a -> Task x b -> Task x b
(&>) t1 t2 =
    t1 `Task.andThen` \_ -> t2


(&>>) : Task x a -> (a -> Task x b) -> Task x b
(&>>) t1 f =
    t1 `Task.andThen` f


init : Task Never (State msg)
init =
    Task.succeed
        { servers = Dict.empty
        , listeners = Dict.empty
        }



-- Cmds


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f cmd =
    case cmd of
        StartServer errorTagger tagger wsPort ->
            StartServer (f << errorTagger) (f << tagger) wsPort

        Send wsPort path id message ->
            Send wsPort path id message

        StopServer wsPort ->
            StopServer wsPort


{-| TODO
-}
startServer : ServerErrorTagger msg -> ServerTagger msg -> WSPort -> Cmd msg
startServer errorTagger tagger wsPort =
    command (StartServer errorTagger tagger wsPort)


{-| TODO
-}
send : WSPort -> Path -> ClientId -> String -> Cmd msg
send wsPort path id message =
    command (Send wsPort path id message)


{-| TODO
-}
stopServer : WSPort -> Cmd msg
stopServer wsPort =
    command (StopServer wsPort)



-- Subs


subMap : (a -> b) -> MySub a -> MySub b
subMap f sub =
    case sub of
        Listen errorTagger sendErrorTagger sendTagger messageTagger connectionTagger wsPort path ->
            Listen (f << errorTagger) (f << sendErrorTagger) (f << sendTagger) (f << messageTagger) (f << connectionTagger) wsPort path


{-| TODO
-}
listen : ListenErrorTagger msg -> SendErrorTagger msg -> SendTagger msg -> MessageTagger msg -> ConnectionTagger msg -> WSPort -> Path -> Sub msg
listen errorTagger sendErrorTagger sendTagger messageTagger connectionTagger wsPort path =
    subscription (Listen errorTagger sendErrorTagger sendTagger messageTagger connectionTagger wsPort path)



-- effect managers API


onEffects : Platform.Router msg Msg -> List (MyCmd msg) -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router cmds newSubs state =
    let
        newSubsDict =
            List.foldl (addMySub router state) Dict.empty newSubs

        oldListeners =
            Dict.diff state.listeners newSubsDict

        newListeners =
            Dict.diff newSubsDict state.listeners

        keepListeners =
            Dict.intersect state.listeners newSubsDict

        handleOneCmd state cmd tasks =
            let
                ( task, newState ) =
                    handleCmd router state cmd
            in
                ( task :: tasks, newState )

        ( tasks, cmdState ) =
            List.foldl (\cmd ( tasks, state ) -> handleOneCmd state cmd tasks) ( [], state ) cmds

        cmdTask =
            Task.sequence (List.reverse <| tasks)
    in
        cmdTask
            &> Task.succeed { cmdState | listeners = Dict.union keepListeners newListeners }


addMySub : Platform.Router msg Msg -> State msg -> MySub msg -> ListenerDict msg -> ListenerDict msg
addMySub router state sub dict =
    case sub of
        Listen errorTagger sendErrorTagger sendTagger messageTagger connectionTagger wsPort path ->
            let
                error msg =
                    errorTagger ( wsPort, path, msg )

                newSub =
                    { sendErrorTagger = sendErrorTagger
                    , sendTagger = sendTagger
                    , messageTagger = messageTagger
                    , connectionTagger = connectionTagger
                    }
            in
                Dict.insert ( wsPort, path ) newSub dict


settings0 : Platform.Router msg Msg -> (a -> Msg) -> Msg -> { onError : a -> Task msg (), onSuccess : Never -> Task x () }
settings0 router errorTagger tagger =
    { onError = \err -> Platform.sendToSelf router (errorTagger err)
    , onSuccess = \_ -> Platform.sendToSelf router tagger
    }


settings1 : Platform.Router msg Msg -> (a -> Msg) -> (b -> Msg) -> { onError : a -> Task Never (), onSuccess : b -> Task x () }
settings1 router errorTagger tagger =
    { onError = \err -> Platform.sendToSelf router (errorTagger err)
    , onSuccess = \result1 -> Platform.sendToSelf router (tagger result1)
    }


settings2 : Platform.Router msg Msg -> (a -> Msg) -> (b -> c -> Msg) -> { onError : a -> Task Never (), onSuccess : b -> c -> Task x () }
settings2 router errorTagger tagger =
    { onError = \err -> Platform.sendToSelf router (errorTagger err)
    , onSuccess = \result1 result2 -> Platform.sendToSelf router (tagger result1 result2)
    }


handleCmd : Platform.Router msg Msg -> State msg -> MyCmd msg -> ( Task Never (), State msg )
handleCmd router state cmd =
    case cmd of
        StartServer errorTagger tagger wsPort ->
            let
                server =
                    Server Nothing errorTagger tagger Dict.empty

                connectCb clientId websocket =
                    Platform.sendToSelf router (Connect wsPort clientId websocket)

                disconnectCb clientId =
                    Platform.sendToSelf router (Disconnect wsPort clientId)

                messageCb path queryString clientId message =
                    Platform.sendToSelf router (Message wsPort path queryString clientId message)
            in
                Maybe.map
                    (\server -> ( Platform.sendToApp router (errorTagger ( wsPort, "Server already exists at specified port: " ++ (toString wsPort) )), state ))
                    (Dict.get wsPort state.servers)
                    // ( Native.Websocket.startServer (settings1 router (ErrorStartServer wsPort) (SuccessStartServer wsPort)) wsPort connectCb disconnectCb messageCb
                       , { state | servers = Dict.insert wsPort server state.servers }
                       )

        Send wsPort path clientId message ->
            ( Maybe.map
                (\server ->
                    Maybe.map (\ws -> Native.Websocket.send (settings0 router (ErrorSend wsPort path clientId) (SuccessSend wsPort path clientId message)) ws message)
                        (Dict.get clientId server.clients)
                        // Platform.sendToSelf router (ErrorSend wsPort path clientId <| "Client does NOT exists with id: " ++ (toString clientId))
                )
                (Dict.get wsPort state.servers)
                // Platform.sendToSelf router (ErrorSend wsPort path clientId <| "Server does NOT exists at specified port: " ++ (toString wsPort))
            , state
            )

        StopServer wsPort ->
            ( Maybe.map
                (\server ->
                    Maybe.map
                        (\wss -> Native.Websocket.stopServer (settings0 router (ErrorStopServer wsPort) (SuccessStopServer wsPort)) wss)
                        server.wsServer
                        // Task.succeed ()
                )
                (Dict.get wsPort state.servers)
                // Platform.sendToSelf router (ErrorStopServer wsPort <| "Server does NOT exists at specified port: " ++ (toString wsPort))
            , state
            )


crashTask : a -> String -> Task Never a
crashTask x msg =
    let
        crash =
            Debug.crash msg
    in
        Task.succeed x


printableState : State msg -> State msg
printableState state =
    state


withServer : State msg -> WSPort -> (Server msg -> Task Never (State msg)) -> Task Never (State msg)
withServer state wsPort f =
    Maybe.map f (Dict.get wsPort state.servers)
        /!/ (\_ -> (crashTask state <| "Server on port " ++ (toStringF wsPort) ++ " is not in state: " ++ (toStringF <| printableState state)))


getServer : State msg -> WSPort -> Server msg
getServer state wsPort =
    case Dict.get wsPort state.servers of
        Just server ->
            server

        Nothing ->
            Debug.crash "error"


withListenerTaggers : State msg -> WSPort -> Maybe Path -> (List (ListenerTaggers msg) -> Task Never (State msg)) -> Task Never (State msg)
withListenerTaggers state wsPort maybePath f =
    Dict.toList state.listeners
        |> List.filter (\( ( wsPort', path ), taggers ) -> wsPort' == wsPort && Maybe.map (\path' -> path' == path) maybePath // True)
        |> List.map snd
        |> f


updateServer : WSPort -> Server msg -> State msg -> State msg
updateServer wsPort newServer state =
    { state | servers = Dict.insert wsPort newServer state.servers }


removeServer : WSPort -> State msg -> State msg
removeServer wsPort state =
    { state | servers = Dict.remove wsPort state.servers }


toListeners : Platform.Router msg Msg -> State msg -> (ListenerTaggers msg -> msg) -> List (ListenerTaggers msg) -> Task Never (State msg)
toListeners router state msgConstructor listenerTaggersList =
    (listenerTaggersList
        |> List.foldl (\listenerTaggers tasks -> Platform.sendToApp router (msgConstructor listenerTaggers) :: tasks) []
        |> Task.sequence
    )
        &> Task.succeed state


type Msg
    = SuccessStartServer WSPort WebsocketServer
    | ErrorStartServer WSPort String
    | Connect WSPort ClientId Websocket
    | Disconnect WSPort ClientId
    | Message WSPort Path QueryString ClientId String
    | ErrorSend WSPort Path ClientId String
    | SuccessSend WSPort Path ClientId String
    | SuccessStopServer WSPort
    | ErrorStopServer WSPort String


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
    case selfMsg of
        SuccessStartServer wsPort websocketServer ->
            let
                process server =
                    Platform.sendToApp router (server.tagger ( wsPort, Started ))
                        &> Task.succeed (updateServer wsPort { server | wsServer = Just websocketServer } state)
            in
                withServer state wsPort process

        ErrorStartServer wsPort err ->
            let
                process server =
                    Platform.sendToApp router (server.errorTagger ( wsPort, err ))
                        &> Task.succeed (removeServer wsPort state)
            in
                withServer state wsPort process

        Connect wsPort clientId websocket ->
            let
                process server =
                    let
                        newState =
                            updateServer wsPort { server | clients = Dict.insert clientId websocket server.clients } state
                    in
                        withListenerTaggers
                            newState
                            wsPort
                            Nothing
                            (toListeners router newState (\listenerTaggers -> listenerTaggers.connectionTagger ( wsPort, clientId, Connected )))
            in
                withServer state wsPort process

        Disconnect wsPort clientId ->
            let
                server =
                    getServer state wsPort

                newState =
                    Maybe.map
                        (\server ->
                            { state | servers = Dict.insert wsPort { server | clients = Dict.remove clientId server.clients } state.servers }
                        )
                        (Dict.get clientId state.servers)
                        // state
            in
                withListenerTaggers
                    state
                    wsPort
                    Nothing
                    (toListeners router newState (\listenerTaggers -> listenerTaggers.connectionTagger ( wsPort, clientId, Disconnected )))

        Message wsPort path queryString clientId message ->
            withListenerTaggers
                state
                wsPort
                (Just path)
                (toListeners router state (\listenerTaggers -> listenerTaggers.messageTagger ( clientId, queryString, message )))

        ErrorSend wsPort path clientId error ->
            let
                error =
                    "Send error: '" ++ error ++ "' for server on port: " ++ (toString wsPort) ++ "for clientId: " ++ (toString clientId)
            in
                withListenerTaggers
                    state
                    wsPort
                    (Just path)
                    (toListeners router state (\listenerTaggers -> listenerTaggers.sendErrorTagger ( wsPort, clientId, error )))

        SuccessSend wsPort path clientId message ->
            withListenerTaggers
                state
                wsPort
                (Just path)
                (toListeners router state (\listenerTaggers -> listenerTaggers.sendTagger ( wsPort, clientId, message )))

        SuccessStopServer wsPort ->
            let
                process server =
                    Platform.sendToApp router (server.tagger ( wsPort, Stopped ))
                        &> Task.succeed (updateServer wsPort { server | wsServer = Nothing } state)
            in
                withServer state wsPort process

        ErrorStopServer wsPort err ->
            let
                process server =
                    Platform.sendToApp router (server.errorTagger ( wsPort, err ))
                        &> Task.succeed (removeServer wsPort state)
            in
                withServer state wsPort process
