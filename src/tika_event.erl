-module(tika_event).
-behaviour(gen_server).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% custom interfaces
-export([json2event/1, 
			json2day/1, 
			event2json/1,
			event2json/2,
			create/1, 
			update/1, 
			findBy/2,
			list/0,
			remove/1,
			updateCreated/1
			]).

%% default interfaces
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).

-include("records.hrl").

-type event() :: #event {}.
-type user() :: #user {}.
-type day() :: #day {}.




%% Interfaces

-spec start() -> {ok, pid()} | {error, any()}.
start()-> gen_server:start_link({local,?MODULE},?MODULE,[], []).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop()-> gen_server:call(?MODULE, stop).


-spec json2event(tuple()) -> event().
json2event(Json) -> gen_server:call(?MODULE,{json2event,Json}).

-spec json2day(tuple()) -> day().
json2day(Json) -> gen_server:call(?MODULE,{json2day,Json}).

-spec event2json(event()) -> tuple().
event2json(Event) -> gen_server:call(?MODULE,{event2json,Event}).

-spec event2json(event(),atom()) -> tuple().
event2json(inc_dates,Event) -> gen_server:call(?MODULE,{event2json,inc_dates,Event}).

-spec create(event()) -> event().
create(Event) -> gen_server:call(?MODULE,{create,Event}).

-spec findBy(atom(),user()) -> list().
findBy(user,User) -> gen_server:call(?MODULE,{findBy,user,User});

findBy(user_answered,User) -> gen_server:call(?MODULE,{findBy,user,User}).

-spec list() -> list().
list() -> gen_server:call(?MODULE,{list}).

-spec update(event()) -> event().
update(Event) -> gen_server:call(?MODULE,{update,Event}).

-spec remove(all) -> list().
remove(all) -> gen_server:call(?MODULE,{remove,all}).



updateCreated(Event) ->
	case Event#event.created of
		undefined -> Event;
		_ -> update(Event#event{created=Event#event.created*1000})
	end.
%% Internal functions


json2day_(Json)->
	case Json of 
		false -> false; %% if it is appointment 0
		_ -> 
			{[
			  {_,Day},
			  {_,Timestamp},
			  {_,Checked},
			  {_,Guests}
			 ]}=Json,
			 #day{
			 	day=binary_to_list(Day),
			 	timestamp=Timestamp,
			 	checked= Checked,
			 	guests=[tika_user:json2user(User) || User <- Guests]
			 }
	end.

day2json(Day)->
	case Day of 
		false -> false;
		_-> {[
			  {<<"day">>,
			  			 case Day#day.day of
			  			 	undefined -> <<"">>;
			  			 	_ -> list_to_binary(Day#day.day)
			  			 end
			  },
			  {<<"timestamp">>,Day#day.timestamp},
			  {<<"checked">>,Day#day.checked},
			  {<<"guests">>,[tika_user:user2json(User) || User <- Day#day.guests]}
			]}
	end.



%% gen_server
init([]) ->
	%mnesia:wait_for_tables([unique_ids,event],20000),
    {ok, ?MODULE}.

handle_call({json2event,Json}, _From, Tab) ->
	{[
	  {_,Id},
	  {_,Title},
	  {_,Description},
	  {_,Dates},
	  {_,Answers},
	  {_,Contacts},
	  {_,Appointment},
	  {_,Answer},
	  {_,Creator}
	 ]}=Json,
	{reply, 
		#event{
			id=Id,
			title=binary_to_list(Title),
			description=binary_to_list(Description),
			dates=[json2day_(Day) ||Day <- Dates],
			answers=Answers,
			contacts = [tika_user:json2user(User) || User <- Contacts],
			appointment = json2day_(Appointment),
			answer = Answer,
			creator = tika_user:json2user(Creator)
		}
	, Tab};

handle_call({json2day,Json}, _From, Tab) ->
	{reply, 
		json2day_(Json)
	, Tab};

handle_call({event2json,Event}, _From, Tab) ->
{reply, 
		{[
	  		{<<"id">>,Event#event.id},
	  		{<<"title">>,list_to_binary(Event#event.title)},
	  		{<<"description">>,list_to_binary(Event#event.description)},
	  		{<<"dates">>,[day2json(Day)||Day<-Event#event.dates]},
	  		{<<"answers">>,Event#event.answers},
	  		{<<"contacts">>,[tika_user:user2json(User)||User<-Event#event.contacts]},
	  		{<<"appointment">>,day2json(Event#event.appointment)},
	  		{<<"answer">>,Event#event.answer},
	  		{<<"creator">>,tika_user:user2json(Event#event.creator)}
	 	]}
	, Tab};

handle_call({event2json,inc_dates,Event}, _From, Tab) ->
{reply, 
		{[
	  		{<<"id">>,Event#event.id},
	  		{<<"title">>,list_to_binary(Event#event.title)},
	  		{<<"description">>,list_to_binary(Event#event.description)},
	  		{<<"dates">>,[day2json(Day)||Day<-Event#event.dates]},
	  		{<<"answers">>,Event#event.answers},
	  		{<<"contacts">>,[tika_user:user2json(User)||User<-Event#event.contacts]},
	  		{<<"appointment">>,day2json(Event#event.appointment)},
	  		{<<"answer">>,Event#event.answer},
	  		{<<"creator">>,tika_user:user2json(Event#event.creator)},
	  		{<<"created">>,Event#event.created}
	 	]}
	, Tab};


handle_call({create,Event}, _From, Tab) -> 
	[NewEvent]=tika_database:create(event,Event#event{created=tika_database:unixTS()}),
	{reply, NewEvent, Tab};

handle_call({findBy,user,User}, _From, Tab) -> 
	Filter= fun(R) ->
		IsUserContact=[X || X <- R#event.contacts, 
							X#user.mail==User#user.mail],
		length(IsUserContact) == 1
	end,
	{reply, tika_database:find(event,Filter), Tab};

handle_call({findBy,user_answered,User}, _From, Tab) -> 
	Filter= fun(R) ->
		IsUserContact=[X || X <- R#event.contacts, 
							X#user.mail==User#user.mail],
		case length(IsUserContact) of
			1 ->
				length([ Date || Date <- R#event.dates,
								length(
									[ Contact || Contact <-Date#day.guests, 
										Contact#user.mail==User#user.mail
									]
								)>0 
				]) > 0; 
			_ -> false
		end
	end,
	{reply, tika_database:find(event,Filter), Tab};

handle_call({list}, _From, Tab) -> 
	{reply, tika_database:find(event), Tab};	

handle_call({update,Event}, _From, Tab) -> 
	[NewEvent]=tika_database:write(event,Event),
	{reply, NewEvent, Tab};

handle_call({remove,all}, _From, Tab) -> 
	{reply, 
		[tika_database:delete(event, Event) || Event <- tika_database:find(event)], 
	Tab};

handle_call(stop, _From, Tab) ->
	{stop, normal, stopped, Tab}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

