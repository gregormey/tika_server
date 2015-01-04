-module(tika_user_tests).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").

user()->
	#user{
		displayName = "Gregor Meyenberg",
		mail="gregor@meyenberg.de"
	}.

event()->
	#event{
		title="Test Event"
	}.

invite_test() ->
	ok=application:start(asn1),
	ok=application:start(public_key),
	ok=application:start(ssl),
	Reply=tika_user:invite(user(),event()),
	?_assert(string:str(binary_to_list(Reply) , "OK")>=0).





