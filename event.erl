-module(event).
-export([add_user_to_event/3]).
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

	

