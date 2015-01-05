-module(tika_user_fsm_tests).
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


fsm_event_test_() ->
	{foreach,
		fun start/0,
		fun stop/1,
		[
		fun (SetupData) ->
			[
				invite(SetupData),
				register(SetupData)
			]
		end,
		fun (SetupData) ->
			[
				register(SetupData)
			]
		end
		]
	}.

start() ->
	{ok, Pid} = tika_user_fsm:start_link(user()),
	Pid.
 
stop(Pid) ->
	tika_user_fsm:stop(Pid,cancel).

invite(Pid) ->
	?_assert(ok == 
	tika_user_fsm:invite(Pid,{event()})).

register(Pid) ->
	?_assert(ok == 
		tika_user_fsm:register(Pid)).