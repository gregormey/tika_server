-module(event_tests).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").

day(Ts)->
	#day{timestamp = Ts}.

event()->
	#event{
		dates = [day("123"),day("456")]
	}.

user()->
	#user{
		displayName = "Gregor Meyenberg"
	}.

add_user_to_event_test() ->
	#event{
			dates=[_,#day{
						guests=[#user{
									displayName="Gregor Meyenberg"
								}]
						}
					] 
		}= event:add_user_to_event(event(),user(),"456").