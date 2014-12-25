-module(event_tests).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").

day(Ts)->
	#day{timestamp = Ts}.

event()->
	#event{
		dates = [day("123"),day("456")]
	}.

user(Name)->
	#user{
		displayName = Name
	}.

add_user_to_event_test() ->
	#event{
			dates=[_,#day{
						guests=[#user{
									displayName="Gregor Meyenberg"
								}]
						}
					] 
		}= event:add_user_to_event(event(),user("Gregor Meyenberg"),"456").

remove_user_from_event_test() ->
	 Event1= event:add_user_to_event(event(),user("Gregor Meyenberg"),"456"),
	 Event2= event:add_user_to_event(Event1,user("Maike Meyenberg"),"456"),
	 #event{
	 	dates=[_,#day{
	 					guests=[#user{
	 								displayName="Maike Meyenberg"
	 							}]
	 					}
	 				] 
	 }= event:remove_user_from_event(Event2,user("Gregor Meyenberg"),"456").