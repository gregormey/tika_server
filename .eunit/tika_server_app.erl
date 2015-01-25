-module(tika_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% include for eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->	 
	Dispatch = cowboy_router:compile([
	{'_', [
		{"/websocket",tika_websocket, []}
	]}
	]),
	%%websocket port
	Port = 10010,
	{ok, _} = cowboy:start_http(http, 100, [{port, Port}],
		[{env, [{dispatch, Dispatch}]}]),
    tika_server_sup:start_link().

stop(_State) ->
    ok.
