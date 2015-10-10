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
	start_websocket(),	
	start_rest(), 
    tika_server_sup:start_link().

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

start_rest()->
	 %% Define static directory for application
   	Opts = [{static_dir, {'_', {priv_dir, ?MODULE, "var/www"}}}],
	leptus:start_listener(http, [{'_', [{tika_rest, undefined_state}]}],Opts).

stop(_State) ->
    ok.
