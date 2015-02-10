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
-export([json2event/1, event2json/1 ,create/1]).

%% default interfaces
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).


-include("records.hrl").

-type event() :: #event {}.


%% Interfaces

-spec start() -> {ok, pid()} | {error, any()}.
start()-> gen_server:start_link({local,?MODULE},?MODULE,[], []).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop()-> gen_server:call(?MODULE, stop).


-spec json2event(tuple()) -> event().
json2event(Json) -> gen_server:call(?MODULE,{json2event,Json}).


-spec event2json(event()) -> tuple().
event2json(Event) -> gen_server:call(?MODULE,{event2json,Event}).

-spec create(event()) -> event().
create(Event) -> gen_server:call(?MODULE,{create,Event}).


%% Internal functions
json2day(Json)->
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
			  {<<"day">>,list_to_binary(Day#day.day)},
			  {<<"timestamp">>,Day#day.timestamp},
			  {<<"checked">>,Day#day.checked},
			  {<<"guests">>,[tika_user:user2json(User) || User <- Day#day.guests]}
			]}
	end.


json2answer(Json) ->
	{[
	  {_,Positive},
	  {_,Negative}
	 ]}=Json,
	 
	#answers {
	 	positive = Positive,
	 	negative = Negative
	}.

answers2json(Answers) ->
	{[
	  	{<<"positive">>,Answers#answers.positive},
		{<<"negative">>,Answers#answers.negative}
	]}.

%% gen_server
init([]) ->
	mnesia:wait_for_tables([unique_ids,event],20000),
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
			dates=[json2day(Day) ||Day <- Dates],
			answers=json2answer(Answers),
			contacts = [tika_user:json2user(User) || User <- Contacts],
			appointment = json2day(Appointment),
			answer = Answer,
			creator = tika_user:json2user(Creator)
		}
	, Tab};

handle_call({event2json,Event}, _From, Tab) ->
{reply, 
		{[
	  		{<<"id">>,Event#event.id},
	  		{<<"title">>,list_to_binary(Event#event.title)},
	  		{<<"description">>,list_to_binary(Event#event.description)},
	  		{<<"dates">>,[day2json(Day)||Day<-Event#event.dates]},
	  		{<<"answers">>,answers2json(Event#event.answers)},
	  		{<<"contacts">>,[tika_user:user2json(User)||User<-Event#event.contacts]},
	  		{<<"appointment">>,day2json(Event#event.appointment)},
	  		{<<"answer">>,Event#event.answer},
	  		{<<"creator">>,tika_user:user2json(Event#event.creator)}
	 	]}
	, Tab};


handle_call({create,Event}, _From, Tab) -> 
	[NewEvent]=tika_database:create(event,Event#event{created=tika_database:unixTS()}),
	tika_process:reg(event,NewEvent),
	{reply, NewEvent, Tab};

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
%% LEGACY
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



