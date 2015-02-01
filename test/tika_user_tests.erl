-module(tika_user_tests).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").

user()->
	#user{id=1,displayName = "Maike Meyenberg",mail="maike@meyenberg.de"}.

start() ->
	stopped=tika_database:install(test),
	tika:start_server(test).
 
stop(Pid) ->
	ok.

tika_database_test_() ->
	{foreach,
		fun start/0,
		fun stop/1,
		[
		fun () ->
			[
				create(),
				user2json(),
				json2user(),
				load(),
				update()
			]
		end
		]
	}.

create() ->
	User=tika_user:create(),
	?assert(1==User#user.id).

user2json()->
	{[
	  {<<"id">>,1},
	  {<<"displayName">>,<<"Maike Meyenberg">>},
	  {<<"mail">>,<<"maike@meyenberg.de">>}
	 ]}=tika_user:user2json(user()).

json2user()->
	User=user(),
	UserJson=tika_user:json2user(
		{[
	  		{<<"id">>,1},
	  		{<<"displayName">>,<<"Maike Meyenberg">>},
	  		{<<"mail">>,<<"maike@meyenberg.de">>}
	 	]}
	),
	User=UserJson.

load()->
	User=tika_user:load(user()),
	?assert(1==User#user.id).

update()->
   User=tika_user:create(),
   Pid=tika_process:id2pid(user,User#user.id),

   User2=tika_user:create(),
   Pid2=tika_process:id2pid(user,User2#user.id),

   ?assert(created==tika_user_fsm:statename(Pid)),
   ?assert(ok==tika_user_fsm:update(Pid,{"test","test"})),
   ?assert(registered==tika_user_fsm:statename(Pid)),
   ?assert(ok==tika_user_fsm:update(Pid,{"test","test"})),

   ?assert(user_exists==tika_user_fsm:update(Pid2,{"test","test"})),
   ?assert(created==tika_user_fsm:statename(Pid2)).





