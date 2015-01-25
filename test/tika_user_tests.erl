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
				load()
			]
		end
		]
	}.

create() ->
	User=tika_user:create(),
	?_assert(1==User#user.id).

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
	?_assert(1==User#user.id).



