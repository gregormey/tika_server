-module(tika_user_tests).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").

user()->
	#user{
		displayName = "Maike Meyenberg",
		mail="maike@meyenberg.de"
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

start() ->
	stopped=tika_database:install(test),
	{ok, Pid} = tika_database:start_link(),
	Pid.
 
stop(Pid) ->
	tika_database:stop().

tika_database_test_() ->
	{foreach,
		fun start/0,
		fun stop/1,
		[
		fun () ->
			[
				create()
			]
		end
		]
	}.

create() ->
	User=tika_user:create(),
	?_assert(1==User#user.id).




