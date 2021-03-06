-module(tika).
-author("Gregor Meyenberg <gregor@meyenberg.de>").

-export([start_server/0]).
-export([stop_server/0]).
-export([restart_server/0]).


%starts tika and dependencie services and database in priv dir
-spec start_server() -> ok.
start_server() ->
	start_applications().

start(App) ->
    start_ok(App, application:start(App, permanent)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) -> 
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) -> 
    erlang:error({app_start_failed, App, Reason}).

stop(App) ->
    stop_ok(App, application:stop(App)).

stop_ok(_App, ok) -> ok;
stop_ok(_App, {error, {not_started, _App}}) -> ok;
stop_ok(App, {error, Reason}) -> 
    erlang:error({app_start_failed, App, Reason}).

%starts all required OTP applications
-spec start_applications() -> ok.
start_applications() ->
	start(tika_server),
	ok.

-spec stop_server() -> ok.
stop_server() ->
	stop(ranch),
	stop(cowboy),
	stop(tika_server),
    ok.

-spec restart_server() -> ok.
 restart_server() ->
 	init:restart(),
 	ok.
