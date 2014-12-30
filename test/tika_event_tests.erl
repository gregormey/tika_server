-module(tika_event_tests).
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
	Event=tika_event_fsm:reject_event(event(),user("Maike Meyenberg")),
	Contacts=Event#event.contacts,
	Contacts=[user("Gregor Meyenberg")].

add_user_to_event_test() ->
	Event=tika_event_fsm:add_user_to_event(event(),user("Gregor Meyenberg"),"456"),
	Dates=Event#event.dates,
	[_,Day]=Dates,
	Guests=Day#day.guests,
	Guests=[user("Gregor Meyenberg")].

remove_user_from_event_test() ->
	Event1= tika_event_fsm:add_user_to_event(event(),user("Gregor Meyenberg"),"456"),
	Event2= tika_event_fsm:add_user_to_event(Event1,user("Maike Meyenberg"),"456"),
	Event3= tika_event_fsm:remove_user_from_event(Event2,user("Gregor Meyenberg"),"456"),
	Dates=Event3#event.dates,
	[_,Day]=Dates,
	Guests=Day#day.guests,
	Guests=[user("Maike Meyenberg")].

fix_test() ->
	Event=tika_event_fsm:fix(event(),day("123")),
	Day=day("123"),
	Day=Event#event.appointment.

edit_title_description_test() ->
	%% Title, Description
	Event=tika_event_fsm:edit(event(),{title,"Test Title"}),
	Event2=tika_event_fsm:edit(Event,{description,"Test Description"}),
	"Test Title"= Event2#event.title,
	"Test Description"= Event2#event.description.

edit_contacts_test() ->	
	%%Contacts
	Event3=tika_event_fsm:edit(event(),{addContact,user("Benjamin Meyenberg")}),
	Contacts1=Event3#event.contacts,
	Contacts1=[user("Gregor Meyenberg"),user("Maike Meyenberg"),user("Benjamin Meyenberg")],
	Event4=tika_event_fsm:edit(Event3,{removeContact,user("Benjamin Meyenberg")}),
	Contacts2=Event4#event.contacts,
	Contacts2=[user("Gregor Meyenberg"),user("Maike Meyenberg")].

edit_dates_test() ->	
	%%Dates
	Event3=tika_event_fsm:edit(event(),{addDate,day("789")}),
	Dates1=Event3#event.dates,
	Dates1=[day("123"),day("456"),day("789")],
	Event4=tika_event_fsm:edit(Event3,{removeDate,day("789")}),
	Dates2=Event4#event.dates,
	Dates2=[day("123"),day("456")].






