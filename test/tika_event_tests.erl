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
		answers = 0,
		contacts = [
			#user {
					id = 11,
					displayName = "Gregor",
					mail = "gregor@meyenberg.de"
				},
			#user {
					id = 12,
					displayName = "Maike",
					mail = "maike@meyenberg.de"
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
      "\"answers\": 0,"++
      "\"contacts\":["++  
         "{  "++
            "\"id\":11,"++
            "\"displayName\":\"Gregor\","++
            "\"mail\":\"gregor@meyenberg.de\""++
         "},"++
         "{  "++
            "\"id\":12,"++
            "\"displayName\":\"Maike\","++
            "\"mail\":\"maike@meyenberg.de\""++
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
 
stop(_Pid) ->
	tika:stop_server(),
	ok.

tika_database_test_() ->
	{foreach,
		fun start/0,
		fun stop/1,
		[
		fun () ->
			[
				%json2event(),
				%event2json(),
				%findBy(),
				%invite(),
				update_dates()%,
				%update_contacts()
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

invite()->
	tika_user:create(#user{displayName = "Maike",
							mail="maike@meyenberg.de",
							registered=tika_database:unixTS()}),

	Event=tika_event:create(event()),
	[Gregor,Maike]= Event#event.contacts,

	Pid=tika_process:id2pid(event,Event#event.id),
	ok=tika_event_fsm:invite(Pid),
	
	%%invite user per mail
	User1=tika_user:load(mail,Gregor#user.mail),
	PidUser1=tika_process:id2pid(user,User1#user.id),
	?assert(invited==tika_user_fsm:statename(PidUser1)),

	%%user is registerd
	User2=tika_user:load(mail,Maike#user.mail),
	PidUser2=tika_process:id2pid(user,User2#user.id),
	?assert(registered==tika_user_fsm:statename(PidUser2)).

findBy() ->
	Event1=tika_event:create(event()),
	tika_event:create(event()),

	[Contact,_]= Event1#event.contacts,

	Events=tika_event:findBy(user,Contact),
	?assert(2==length(Events)).


update_dates() -> 
	Event=tika_event:create(event()),
	Dates=[#day {timestamp = 1421581862589},#day {timestamp = 1426245702443}],
	Pid=tika_process:id2pid(event,Event#event.id),
	ok=tika_event_fsm:update_dates(Pid,{Dates}),
	[UpdatedEvent]=tika_database:find(id,event,Event#event.id),
	[_,#day {guests=Guests}]=UpdatedEvent#event.dates,
	%%expect still one guest after update
	?assert(1==length(Guests)).


update_contacts() -> 
	Event=tika_event:create(event()),
	Contacts=[#user {
					id = 13,
					displayName = "Maike",
					mail = "maike@meyenberg.de"
				},
				#user {
					id = 13,
					displayName = "Benjamin",
					mail = "benjamin@meyenberg.de"
				}],
	Pid=tika_process:id2pid(event,Event#event.id),
	ok=tika_event_fsm:update_contacts(Pid,{Contacts}),
	UpdatedEvent=tika_database:find(id,event,Event#event.id),
	?assert(Contacts==UpdatedEvent#event.contacts),
	[#day {guests=Guests}]=UpdatedEvent#event.dates,

	%%expect zero guest after update
	?assert(0==length(Guests)).










