-module(tika_tests).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").

main_test_() ->
    % sleep for 10 seconds
    {timeout, 60, ?_assertEqual(ok, tika:start_server())}.

%start_server_test() ->
%	ok=tika:start_server().

stop_server_test() ->
	ok=tika:stop_server().


