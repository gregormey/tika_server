-module(tika_event).
-export([add_user_to_event/3, remove_user_from_event/3, 
	reject_event/2,fix/2,edit/2]).
-include("records.hrl").

add_user_to_event(Event=#event{},User=#user{},Day_ts)->
	Dates = Event#event.dates,
	Fun = (fun(Day=#day{})->
				case Day#day.timestamp of
					Day_ts -> 
							Guests = Day#day.guests,
							Day#day{guests = lists:append(Guests,[User])};
					_ -> Day
				end
			end),
	Event#event{dates = lists:map(Fun,Dates)}.


remove_user_from_event(Event=#event{},User=#user{},Day_ts)->
	Dates = Event#event.dates,
	Fun = (fun(Day=#day{})->
				case Day#day.timestamp of
					Day_ts -> 
							Guests = Day#day.guests,
							Day#day{guests = lists:delete(User,Guests)};
					_ -> Day
				end
			end),
	Event#event{dates = lists:map(Fun,Dates)}.

reject_event(Event=#event{},User=#user{}) -> 
	Contacts=Event#event.contacts,
	Event#event{contacts = lists:delete(User,Contacts)}.

fix(Event=#event{},Day=#day{}) ->
	Event#event{appointment=Day}.

%edit Event properties
edit(Event=#event{},{title,Title}) ->
	Event#event{title=Title};
edit(Event=#event{},{description,Description}) ->
	Event#event{description=Description};
edit(Event=#event{},{addContact,User=#user{}}) ->
	Contacts=Event#event.contacts,
	Event#event{contacts = lists:append(Contacts,[User])};
edit(Event=#event{},{removeContact,User=#user{}}) ->
	Contacts=Event#event.contacts,
	Event#event{contacts = lists:delete(User,Contacts)};
edit(Event=#event{},{addDate,Day=#day{}}) ->
	Dates=Event#event.dates,
	Event#event{dates = lists:append(Dates,[Day])};
edit(Event=#event{},{removeDate,Day=#day{}}) ->
	Dates=Event#event.dates,
	Event#event{dates = lists:delete(Day,Dates)}.

	
