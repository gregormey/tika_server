-module(event_fsm_tests).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([event/0, user/1]).

day(Ts)->
	#day{timestamp = Ts}.

user(Name)->
	#user{
		displayName = Name
	}.


event()->
	#event{
		title = "Test Event",
		dates = [day("123"),day("456")],
		contacts = [user("Gregor Meyenberg"),user("Maike Meyenberg")]
	}.

fsm_event_test_() ->
	{setup,
		fun start/0,
		fun stop/1,
		fun (SetupData) ->
			[
			select_date_event(SetupData),
			deselect_date_event(SetupData),
			reject_event1(SetupData),
			reject_event2(SetupData)
			]
		end
	}.

start() ->
	{ok, Pid} = event_fsm:start_link(event()),
	Pid.
 
stop(Pid) ->
	event_fsm:stop(Pid,event_is_over).

select_date_event(Pid) ->
	?_assert(ok == 
	event_fsm:select_date(Pid,{user("Gregor Meyenberg"),"123"})).

deselect_date_event(Pid) ->
	?_assert(ok ==
		event_fsm:deselect_date(Pid,{user("Gregor Meyenberg"),"123"})).
reject_event1(Pid) -> 
	?_assert(ok == 
		event_fsm:reject(Pid,{user("Gregor Meyenberg")})).
reject_event2(Pid) ->
	?_assert(ok == 
		event_fsm:reject(Pid,{user("Maike Meyenberg")})).




