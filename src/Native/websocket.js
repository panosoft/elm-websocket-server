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
const _panosoft$elm_websocket$Native_Websocket = function() {
	// next client id
	var nextClientId = 0;
	//////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Cmds
	const _startServer = (port, connectCb, disconnectCb, cb) => {
		try {
			const wss = new WebSocketServer({port});
			// connect handler
			wss.on('connection', ws => {
				// set clientId since we cannot compare websockets in Elm and we're going to need clientId when disconnected
				const clientId = nextClientId++;
				// disconnect handler
				ws.on('close', _ => {
					disconnectCb(clientId);
				});
				connectCb(clientId, ws);
			});
			// return
			cb(null, wss);
		}
		catch (err) {
			cb(err.message);
		}
	};
	//////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Subs

	//////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Cmds
	const startServer = helper.call3_1(_startServer);
	//////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Subs
//	const listen = helper.call3_1(_listen, helper.unwrap({1:'_0'}));

	return {
		///////////////////////////////////////////
		// Cmds
		startServer: F4(startServer),
		///////////////////////////////////////////
		// Subs
//		listen: F4(listen)
	};

}();
// for testing locally
const _user$project$Native_Websocket = _panosoft$elm_websocket$Native_Websocket;
