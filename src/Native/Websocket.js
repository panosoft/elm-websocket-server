var _panosoft$elm_websocket_server$Native_Websocket;
(function() {
	// Elm globals (some for elm-native-helpers and some for us and some for the future)
	const E = {
		A2: A2,
		A3: A3,
		A4: A4,
		Scheduler: {
			nativeBinding: _elm_lang$core$Native_Scheduler.nativeBinding,
			succeed:  _elm_lang$core$Native_Scheduler.succeed,
			fail: _elm_lang$core$Native_Scheduler.fail,
			rawSpawn: _elm_lang$core$Native_Scheduler.rawSpawn
		},
		List: {
			fromArray: _elm_lang$core$Native_List.fromArray
		},
		Maybe: {
			Nothing: _elm_lang$core$Maybe$Nothing,
			Just: _elm_lang$core$Maybe$Just
		},
		Result: {
			Err: _elm_lang$core$Result$Err,
			Ok: _elm_lang$core$Result$Ok
		}
	};
	// This module is in the same scope as Elm but all modules that are required are NOT
	// So we must pass elm globals to it (see https://github.com/panosoft/elm-native-helpers for the minimum of E)
	const helper = require('@panosoft/elm-native-helpers/helper')(E);
	const WebSocketServer = require('ws').Server;
	const url = require('url');
	_panosoft$elm_websocket_server$Native_Websocket = function() {
		// next client id
		var nextClientId = 0;
		//////////////////////////////////////////////////////////////////////////////////////////////////////////
		// Cmds
		const getFiles = (keyPath, certPath, finalCb) => {
			const fs = require('fs');
			const readFile = (path, cb) => {
				fs.readFile(path, (err, data) => {
					if (err)
						finalCb(err);
					else
						cb(data);
				});
			}
			readFile(keyPath, key => {
				readFile(certPath, cert => {
					finalCb(null, {key, cert});
				});
			});
		};
		const _startServer = (keyPath, certPath, port, startErrorCb, connectCb, disconnectCb, messageCb, cb) => {
			try {
				const httpRequestHandler = (req, res) => {
					res.writeHead(200);
					res.end('Elm Websocket Server');
				};
				const doStart = (err, certData) => {
					try {
						if (err) {
							E.Scheduler.rawSpawn(startErrorCb(err));
							return;
						}
						// start appropriate HTTP Server
						const http = require(`http${certData ? 's' : ''}`);
						const args = [certData, httpRequestHandler].filter(x => x);
						const server = http.createServer.apply(null, args).listen(port, err => {
	                        if (err)
	                            cb(err);
	                        else {
	                            // start websocket server
	        					const wss = new WebSocketServer({server});
	        					wss.__server__ = server;
	        					// connect handler
	        					wss.on('connection', ws => {
									const parsedUrl = url.parse(ws.upgradeReq.url, true);
	        						// set clientId since we cannot compare websockets in Elm and we're going to need clientId when disconnected
	        						const clientId = nextClientId++;
	        						const listener = message => {
	        							E.Scheduler.rawSpawn(A4(messageCb, parsedUrl.pathname, JSON.stringify(parsedUrl.query), clientId, message));
	        						};
	        						// disconnect handler
	        						ws.on('close', _ => {
	        							ws.removeListener('message', listener);
	        							E.Scheduler.rawSpawn(A3(disconnectCb, parsedUrl.pathname, clientId, ws.__ipAddress__));
	        						});
	        						// listen
	        						ws.on('message', listener);
									ws.__ipAddress__ = ws.upgradeReq.headers['x-forwarded-for'] || ws.upgradeReq.connection.remoteAddress;
	        						E.Scheduler.rawSpawn(A4(connectCb, parsedUrl.pathname, clientId, ws.__ipAddress__, ws));
	        					});
	        					// return
	        					cb(null, wss);
	                        }
	                    });
	                    server.on('error', err => {
	                        cb(err.message);
	                    });
					}
					catch (err) {
						cb(err.message);
					}
				};
				// if SSL get certificate files then start WS server otherwise just start it
				if (!!keyPath && !!certPath)
					getFiles(keyPath, certPath, doStart);
				else
					doStart();
			}
			catch (err) {
				cb(err.message);
			}
		};
		const _send = (ws, message, cb) => {
			try {
				ws.send(message, err => err ? cb(err.message) : cb());
			}
			catch (err) {
				cb(err.message);
			}
		};
		const _stopServer = (wss, cb) => {
			try {
				wss.close(err => err ? cb(err.message) : cb());
				wss.__server__.close();
			}
			catch (err) {
				cb(err.message);
			}
		}
		const startServer = helper.call7_1(_startServer, helper.unwrap({1:'_0',2:'_0'}));
		const send = helper.call2_0(_send);
		const stopServer = helper.call1_0(_stopServer);

		return {
			///////////////////////////////////////////
			// Cmds
			startServer: F8(startServer),
			send: F3(send),
			stopServer: F2(stopServer)
			///////////////////////////////////////////
			// Subs
		};

	}();
})();
// for local testing
const _user$project$Native_Websocket = _panosoft$elm_websocket_server$Native_Websocket;
