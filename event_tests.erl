-module(event_tests).
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
		dates = [day("123"),day("456")],
		contacts= [user("Gregor Meyenberg"),user("Maike Meyenberg")]
	}.


reject_event_test() ->
	Event=event:reject_event(event(),user("Maike Meyenberg")),
	Contacts=Event#event.contacts,
	Contacts=[user("Gregor Meyenberg")].

add_user_to_event_test() ->
	Event=event:add_user_to_event(event(),user("Gregor Meyenberg"),"456"),
	Dates=Event#event.dates,
	[_,Day]=Dates,
	Guests=Day#day.guests,
	Guests=[user("Gregor Meyenberg")].

remove_user_from_event_test() ->
	Event1= event:add_user_to_event(event(),user("Gregor Meyenberg"),"456"),
	Event2= event:add_user_to_event(Event1,user("Maike Meyenberg"),"456"),
	Event3= event:remove_user_from_event(Event2,user("Gregor Meyenberg"),"456"),
	Dates=Event3#event.dates,
	[_,Day]=Dates,
	Guests=Day#day.guests,
	Guests=[user("Maike Meyenberg")].

fix_test() ->
	Event=event:fix(event(),day("123")),
	Day=day("123"),
	Day=Event#event.appointment.

edit_test() ->
	Event=event:edit(event(),{title,"Test Title"}),
	Event2=event:edit(Event,{description,"Test Description"}),
	"Test Title"= Event2#event.title,
	"Test Description"= Event2#event.description,
	Event3=event:edit(Event,{addContact,user("Benjamin Meyenberg")}),
	Contacts1=Event3#event.contacts,
	Contacts1=[user("Gregor Meyenberg"),user("Maike Meyenberg"),user("Benjamin Meyenberg")],
	Event4=event:edit(Event3,{removeContact,user("Benjamin Meyenberg")}),
	Contacts2=Event4#event.contacts,
	Contacts2=[user("Gregor Meyenberg"),user("Maike Meyenberg")].



