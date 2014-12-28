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
    tika_server_sup:start_link().

stop(_State) ->
    ok.

%% simple app test
-ifdef(TEST).

simple_test() ->
    ok = application:start(tika_server),
    ?assertNot(undefined == whereis(tika_server_sup)).

-endif.