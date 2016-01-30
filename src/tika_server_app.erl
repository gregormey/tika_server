-module(tika_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start_rest/0]).

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
	{ok,Ip}=inet:parse_address(tika_config:get_value(config,[config,ip], "127.0.0.1")),
	HostMatch=tika_config:get_value(config,[config,hostmatch], '_'), 

	 %% Define static directory for application
   	Opts = [
   			{ip, Ip},
   			{static_dir, {HostMatch, {priv_dir, ?MODULE, "var/www"}}}
   			],
	leptus:start_listener(http, [{HostMatch, [{tika_rest, undefined_state}]}],Opts).

stop(_State) ->
    ok.
