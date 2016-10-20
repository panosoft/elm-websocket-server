# Websocket Server Effects Manager for Elm

> Websocket Server Effects Manager for node Elm programs. It supports both `ws://` and `wss://` protocols. Multiple servers can be started, one per port. Then multiple listeners per port can be created each with a unique path. For example a server can be started on port `8080` and then a listener on path `/` and `/api` can be registered.

> Messages will be routed to the listener along with the Query String of the request.

> This is built on top of the canonical Websocket library for node, [ws](https://github.com/websockets/ws).

## Install

### Elm

Since the Elm Package Manager doesn't allow for Native code and this uses Native code, you have to install it directly from GitHub, e.g. via [elm-github-install](https://github.com/gdotdesign/elm-github-install) or some equivalent mechanism.

### Node modules

You'll also need to install the dependent node modules at the root of your Application Directory. See the example `package.json` for a list of the dependencies.

The installation can be done via `npm install` command.

### Test program

The test server is a simple echo server. Use `buildTest.sh` to build it and run it with `node main` command.

## API

### Commands

> Start a Websocket Server on the specified port with optional certificate files for SSL support

```elm
startServer : ServerErrorTagger msg -> ServerStatusTagger msg -> UnhandledMessageTagger msg -> Maybe FilePath -> Maybe FilePath -> WSPort -> Cmd msg
startServer errorTagger tagger unhandledMessageTagger keyPath certPath wsPort
```
__Usage__

```elm
-- Start a websocket server
startServer ServerError Server UnhandledMessage (Just "/path/to/privateKey.pem") (Just "/path/to/certificate.pem") 8080

-- Start an SSL websocket server
startServer ServerError Server UnhandledMessage Nothing Nothing 8080
```
* `8080` is the port
* `ServerError`, `Server` are the error and success messages sent to the app
* `UnhandledMessage` is the message sent when a message is received on a path without a listener

> Send a message to the client

Send a message to the specified port and client. The `id` is the `ClientId` received from the `listen` subscription when a client connects to the server. N.B. client ids are unique per server, i.e. per port.

```elm
send : SendErrorTagger msg -> SendTagger msg -> WSPort -> ClientId -> String -> Cmd msg
send sendErrorTagger sendTagger wsPort id message
```
__Usage__

```elm
send SendError Sent 8080 1 "{a:1, b:2}"
```
* `SendError` and `Sent` are your application's messages to handle the different scenarios
* `8080` is the servers port
* `1` is the client id that was received from the `listen` subscription when a client connected to the server
* `{a:1, b:2}` is the message being sent

> Stop the server on the specified port

```elm
stopServer : ServerErrorTagger msg -> ServerStatusTagger msg -> WSPort -> Cmd msg
stopServer errorTagger tagger wsPort
```
__Usage__

```elm
stopServer ServerError Server 8080
```

* `ServerError` and `Server` are your application's messages to handle the different scenarios
* `8080` is the server's port


### Subscriptions

> Listen for messages and connections/disconnections

```elm
listen : ListenErrorTagger msg -> MessageTagger msg -> ConnectionStatusTagger msg -> WSPort -> Path -> Sub msg
listen errorTagger messageTagger ConnectionStatusTagger wsPort path
```
__Usage__

```elm
listen ListenError WSMessage Connection 8080 "/auth"
```
* `ListenError` is your application's message to handle an error in listening
* `WSMessage` is your application's message to handle received messages
* `Connection` is your application's message to handle when a client connects or disconnects
* `8080` is the server port
* `/auth` is the path to listen

### Messages

#### ServerErrorTagger

Error when starting/stopping servers.

```elm
type alias ServerErrorTagger msg =
    ( WSPort, String ) -> msg
```

__Usage__

```elm
ServerError ( wsPort, error ) ->
	let
		l =
			Debug.log "ServerError" ( wsPort, error )
	in
		model ! []
```

#### ListenErrorTagger

Error when attempting to listen to a port, path.

```elm
type alias ListenErrorTagger msg =
    ( WSPort, Path, String ) -> msg
```

__Usage__

```elm
ListenError ( wsPort, path, error ) ->
	let
		l =
			Debug.log "ListenError" ( wsPort, path, error )
	in
		{ model | listenError = True } ! []
```

#### SendErrorTagger

Error attempting to send.

```elm
type alias SendErrorTagger msg =
    ( WSPort, ClientId, String ) -> msg
```

__Usage__

```elm
SendError ( wsPort, clientId, error ) ->
	let
		l =
			Debug.log "SendError" ( wsPort, clientId, error )
	in
		model ! []
```

#### SendTagger

Successful send.

```elm
type alias SendTagger msg =
    ( WSPort, ClientId, String ) -> msg
```

__Usage__

```elm
Sent ( wsPort, clientId, message ) ->
	let
		l =
			Debug.log "Send" ( wsPort, clientId, message )
	in
		{ model | receiveCount = model.receiveCount + 1 } ! []
```

#### ServerStatusTagger

Server status.

```elm
type alias ServerStatusTagger msg =
    ( WSPort, ServerStatus ) -> msg
```

__Usage__

```elm
ServerStatus ( wsPort, status ) ->
	let
		l =
			Debug.log "ServerStatus" ( wsPort, status )
	in
		model ! []
```

#### ConnectionStatusTagger

Connection status.

```elm
type alias ConnectionStatusTagger msg =
    ( WSPort, ClientId, ConnectionStatus ) -> msg
```

__Usage__

```elm
ConnectionStatus ( wsPort, clientId, status ) ->
	let
		l =
			Debug.log "ConnectionStatus" ( wsPort, clientId, status )
	in
		model ! []
```

#### MessageTagger

Message received.

```elm
type alias MessageTagger msg =
    ( ClientId, QueryString, String ) -> msg
```

__Usage__

```elm
WSMessage ( clientId, queryString, message ) ->
	let
		l =
			Debug.log "WSMessage" ( clientId, queryString, message )
	in
		model ! []
```

#### UnhandledMessageTagger

Message received on a path without a listener, i.e. unhandled.

```elm
type alias UnhandledMessageTagger msg =
    ( WSPort, Path, QueryString, ClientId, String ) -> msg
```

__Usage__

```elm
UnhandledMessage ( wsPort, path, queryString, clientId, message ) ->
	let
		l =
			Debug.log "UnhandledMessage" ( wsPort, path, queryString, clientId, message )
	in
		model ! []
```


## Warning

This library is still in alpha.
