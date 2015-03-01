-module(tika_event_tests).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").


event()->
	#event{
		id = 28928,
		title = "test eien eVents",
		description = "",
		dates = [
			#day {
				day = "2015-01-18T11:51:02.589Z",
				timestamp = 1421581862589,
				checked = false,
				guests = [
					#user {
						id = 11,
						displayName = "Gregor",
						mail = "gregor@meyenberg.de"
					}
				]
			}
		],
		answers = #answers{
				positive = 0,
				negative = 0
		},
		contacts = [
			#user {
					id = 11,
					displayName = "Gregor",
					mail = "gregor@meyenberg.de"
				}
		],
		appointment = false,
		answer = false,
		creator = #user {
					id = 11,
					displayName = "Gregor",
					mail = "gregor@meyenberg.de"
				}
	}.

getEventJsonStr()->
	"{" ++  
      			"\"id\":28928,"++
      			"\"title\":\"test eien eVents\","++
      			"\"description\":\"\","++
			    "\"dates\":["++  
			         "{"++  
			            "\"day\":\"2015-01-18T11:51:02.589Z\","++
			            "\"timestamp\":1421581862589,"++
			            "\"checked\":false,"++
			            "\"guests\":["++
			            		"{  "++
						            "\"id\":11,"++
						            "\"displayName\":\"Gregor\","++
						            "\"mail\":\"gregor@meyenberg.de\""++
						         "}"++
			            "]"++
			         "}"++
			      "],"++
      "\"answers\":{  "++
         "\"positive\":0,"++
         "\"negative\":0"++
      "},"++
      "\"contacts\":["++  
         "{  "++
            "\"id\":11,"++
            "\"displayName\":\"Gregor\","++
            "\"mail\":\"gregor@meyenberg.de\""++
         "}"++
      "],"++
      "\"appointment\":false,"++
      "\"answer\":false,"++
      "\"creator\":{ "++ 
         "\"id\":11,"++
         "\"displayName\":\"Gregor\","++
         "\"mail\":\"gregor@meyenberg.de\""++
      "}}".

start() ->
	stopped=tika_database:install(test),
	tika:start_server(test).
 
stop(Pid) ->
	tika:stop_server(),
	ok.

tika_database_test_() ->
	{foreach,
		fun start/0,
		fun stop/1,
		[
		fun () ->
			[
				json2event(),
				event2json(),
				create()
			]
		end
		]
	}.

json2event()->
	Event=event(),
	EventJson=tika_event:json2event(
		jiffy:decode(
			getEventJsonStr()	
		)
	),
	Event=EventJson.

event2json()->
	EventJson1=tika_event:event2json(event()),
	EventJson2=jiffy:decode(getEventJsonStr()),
	EventJson1=EventJson2.

create()->
	Event=tika_event:create(event()),
	Pid=tika_process:id2pid(event,Event#event.id),
	tika_event_fsm:invite(Pid).



