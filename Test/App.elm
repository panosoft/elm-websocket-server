port module Test.App exposing (..)

import Html exposing (..)
import Html.App
import Time exposing (..)
import Websocket exposing (..)
import Listener


port node : Float -> Cmd msg


type alias Model =
    { wsPort : WSPort
    , path : String
    , receiveCount : Int
    , listenError : Bool
    , listenerModel : Listener.Model
    , stopped : Bool
    }


type Msg
    = Nop
    | StopServer Time
    | ServerError ( WSPort, String )
    | Server ( WSPort, ServerStatus )
    | UnhandledMessage ( WSPort, Path, QueryString, ClientId, String )
    | ListenModule Listener.Msg


wsPort : Int
wsPort =
    8080


initModel : Model
initModel =
    { wsPort = wsPort
    , path = "/"
    , receiveCount = 0
    , listenError = False
    , listenerModel = Listener.initModel wsPort
    , stopped = False
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            initModel
    in
        -- model ! [ Websocket.startServer ServerError Server UnhandledMessage (Just "/Users/charles/Documents/devCerts/privateKey.pem") (Just "/Users/charles/Documents/devCerts/certificate.pem") model.wsPort ]
        model ! [ Websocket.startServer ServerError Server UnhandledMessage Nothing Nothing model.wsPort ]


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

        StopServer _ ->
            let
                l =
                    if model.stopped then
                        ""
                    else
                        Debug.log "Stopping Server" "..."

                cmd =
                    if model.stopped then
                        Cmd.none
                    else
                        Websocket.stopServer ServerError Server model.wsPort

                {- }, node 1 -}
            in
                { model | stopped = True } ! [ cmd ]

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
                model ! []

        UnhandledMessage ( wsPort, path, queryString, clientId, message ) ->
            let
                l =
                    Debug.log "UnhandledMessage" ( wsPort, path, queryString, clientId, message )
            in
                model ! []

        ListenModule msg ->
            let
                ( newListenerModel, cmd ) =
                    Listener.update msg model.listenerModel
            in
                { model | listenerModel = newListenerModel } ! [ Cmd.map ListenModule cmd ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ListenModule <| Listener.subscriptions model.listenerModel
          -- , Time.every (15 * second) StopServer
        ]
