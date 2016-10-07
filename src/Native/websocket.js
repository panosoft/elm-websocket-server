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
const _panosoft$elm_websocket$Native_Websocket = function() {
	// next client id
	var nextClientId = 0;
	//////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Cmds
	const _startServer = (port, connectCb, disconnectCb, messageCb, cb) => {
		try {
			const wss = new WebSocketServer({port});
			// connect handler
			wss.on('connection', ws => {
				// set clientId since we cannot compare websockets in Elm and we're going to need clientId when disconnected
				const clientId = nextClientId++;
				const listener = message => {
					const parsedUrl = url.parse(ws.upgradeReq.url, true);
					E.Scheduler.rawSpawn(A4(messageCb, parsedUrl.pathname, JSON.stringify(parsedUrl.query), clientId, message));
				};
				// disconnect handler
				ws.on('close', _ => {
					ws.removeListener('message', listener);
					E.Scheduler.rawSpawn(disconnectCb(clientId));
				});
				// listen
				ws.on('message', listener);
				E.Scheduler.rawSpawn(A2(connectCb, clientId, ws));
			});
			// return
			cb(null, wss);
		}
		catch (err) {
			cb(err.message);
		}
	};
	const _send = (ws, message, cb) => {
		try {
			ws.send(message, cb);
		}
		catch (err) {
			cb(err.message);
		}
	};
	const _stopServer = (wss, cb) => {
		try {
			wss.close(cb);
		}
		catch (err) {
			cb(err.message);
		}
	}
	//////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Subs
	//////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Cmds
	const startServer = helper.call4_1(_startServer);
	const send = helper.call2_0(_send);
	const stopServer = helper.call1_0(_stopServer);

	return {
		///////////////////////////////////////////
		// Cmds
		startServer: F5(startServer),
		send: F3(send),
		stopServer: F2(stopServer)
		///////////////////////////////////////////
		// Subs
	};

}();
// for local testing
const _user$project$Native_Websocket = _panosoft$elm_websocket$Native_Websocket;
