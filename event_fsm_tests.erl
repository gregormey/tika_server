-module(event_fsm_tests).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").


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
	{foreach,
		fun start/0,
		fun stop/1,
		[
		fun (SetupData) ->
			[
			editTitle(SetupData),
			editDescription(SetupData),
			addContact(SetupData),
			removeContact(SetupData),
			confirm_date_event(SetupData),
			deconfirm_date_event(SetupData),
			reject_event1(SetupData),
			reject_event2(SetupData)
			]
		end,
		fun (SetupData) ->
			[
				fix(SetupData),
				over(SetupData)
			]
		end
		]
	}.

start() ->
	{ok, Pid} = event_fsm:start_link(event()),
	Pid.
 
stop(Pid) ->
	event_fsm:stop(Pid,cancel).

confirm_date_event(Pid) ->
	?_assert(ok == 
	event_fsm:confirm_date(Pid,{user("Gregor Meyenberg"),"123"})).

deconfirm_date_event(Pid) ->
	?_assert(ok ==
		event_fsm:deconfirm_date(Pid,{user("Gregor Meyenberg"),"123"})).
reject_event1(Pid) -> 
	?_assert(ok == 
		event_fsm:reject(Pid,{user("Gregor Meyenberg")})).
reject_event2(Pid) ->
	?_assert(ok == 
		event_fsm:reject(Pid,{user("Maike Meyenberg")})).
fix(Pid) ->
	?_assert(ok == 
		event_fsm:fix(Pid,{day("123")})).
over(Pid) ->
	?_assert(ok == 
		event_fsm:over(Pid)).
editTitle(Pid) ->
	?_assert(ok == 
		event_fsm:edit(Pid,{title,"Test Title"})).
editDescription(Pid) ->
	?_assert(ok == 
		event_fsm:edit(Pid,{description,"Test Description"})).
addContact(Pid) ->
	?_assert(ok == 
		event_fsm:edit(Pid,{addContact,user("Benjamin Meyenberg")})).
removeContact(Pid) ->
	?_assert(ok == 
		event_fsm:edit(Pid,{removeContact,user("Benjamin Meyenberg")})).




