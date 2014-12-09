-module(event).


-include("records.hrl").

find_day_by_ts([],Day_ts) ->
	{ts_not_found};	

find_day_by_ts([Day|Days],Day_ts)->
	case Day#day.timestamp of
		Day_ts -> Day 		 
		_ -> add_user_to_day(Days,User,Day_ts)
	end.

add_user_to_event(Event=#event{},User=#user{},Day_ts)->
	Dates = Event=#event.dates
	Day = find_day_by_ts(Dates,Day_ts),
	Guests = Day#day.guests,
	NewDay = Day#day{guests = [Guests | User]},
	Event#{dates = [ Date || Date <-Dates , Date  =/= Day | NewDay]}.

	

