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
	%% @TODO: check if mnesia shema exists and install in case	 
	start_appications(),
	start_websocket(),
    tika_server_sup:start_link().

start_appications()->
	application:start(mnesia),
	application:start(asn1),
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(gproc),
	application:start(cowlib),
	application:start(ranch),
	application:start(cowboy).

start_websocket()->
	Dispatch = cowboy_router:compile([
	{'_', [
		{"/websocket",tika_websocket, []}
	]}
	]),
	%%websocket port
	Port = 10010,
	{ok, _} = cowboy:start_http(http, 100, [{port, Port}],
		[{env, [{dispatch, Dispatch}]}]).

stop_applications()->
	application:stop(mnesia),
	application:stop(asn1),
	application:stop(crypto),
	application:stop(public_key),
	application:stop(ssl),
	application:stop(gproc),
	application:stop(cowlib),
	application:stop(ranch),
	application:stop(cowboy).


stop(_State) ->
	stop_applications(),
    ok.
