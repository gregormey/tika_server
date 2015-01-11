-module(tika).
-author("Gregor Meyenberg <gregor@meyenberg.de>").

-export([start_server/1]).
-export([start_server/0]).
-export([stop_server/0]).
-export([restart_server/0]).
-export([set_dbPath/0]).

%starts tika and dependencie services with a path to database
-spec start_server(test) -> ok.
start_server(test) ->
	start_applications().

%starts tika and dependencie services and database in priv dir
-spec start_server() -> ok.
start_server() ->
	set_dbPath(),
	start_applications().

%sets mnesia dir
-spec set_dbPath() -> ok.
set_dbPath()->
	application:set_env(mnesia, dir, filename:join("./priv", "data")).

%starts all required OTP applications
-spec start_applications() -> ok.
start_applications() ->
	ok=application:start(asn1),
	application:start(crypto),
	ok=application:start(public_key),
	ok=application:start(ssl),
	ok=application:start(gproc),
	ok=application:start(cowlib),
	ok=application:start(ranch),
	ok=application:start(cowboy),
	%ok=application:start(leptus),
	ok=application:start(tika_server),
	%leptus:start_http(tika_server),
	ok.

-spec stop_server() -> ok.
stop_server() ->
    init:stop(),
    ok.

-spec restart_server() -> ok.
 restart_server() ->
 	init:restart(),
 	ok.