-module(tika_user).
-behaviour(gen_server).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% custom interfaces
-export([create/0,create/1,load/1,load/2,user2json/1,json2user/1]).

%% default interfaces
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).


-include("records.hrl").

-type user() :: #user {}.
-type event() :: #event {}.

%% Interfaces

-spec start() -> {ok, pid()} | {error, any()}.
start()-> gen_server:start_link({local,?MODULE},?MODULE,[], []).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop()-> gen_server:call(?MODULE, stop).

-spec create() -> user().
create() -> gen_server:call(?MODULE,create).

-spec create(user()) -> user().
create(User) -> gen_server:call(?MODULE,{create,User}).

-spec load(user()) -> user()| not_found.
load(User) -> gen_server:call(?MODULE,{load,User}).

-spec load(mail,string()) -> user()| not_found.
load(mail,Mail) -> gen_server:call(?MODULE,{load,mail,Mail}).


-spec user2json(user()) -> tuple().
user2json(User) -> gen_server:call(?MODULE,{user2json,User}).

-spec json2user(tuple()) -> user().
json2user(Json) -> gen_server:call(?MODULE,{json2user,Json}).

%% Internal functions



%% gen_server
init([]) ->
	mnesia:wait_for_tables([unique_ids,user],20000),
    {ok, ?MODULE}.

%% call handler to create user with a new id 
handle_call(create, _From, Tab) ->
	[User]=tika_database:create(user,#user{created=tika_database:unixTS()}),
	{reply, User, Tab};

handle_call({create,User}, _From, Tab) ->
	[NewUser]=tika_database:create(user,User#user{created=tika_database:unixTS()}),
	{reply, NewUser, Tab};

handle_call({load,User}, _From, Tab) ->
	{reply, 
		case tika_database:find(id,user,User#user.id) of
			not_found -> not_found;
			[FoundUser] -> FoundUser 
		end
	, Tab};


handle_call({load,mail,Mail}, _From, Tab) ->
	Fun=fun(R) ->
			Mail==R#user.mail
	end,
	{reply,
		case tika_database:find(user,Fun) of
			not_found -> not_found;
			[FoundUser] -> FoundUser
		end
	 , Tab};

handle_call({json2user,Json}, _From, Tab) ->
	{[
	  {_,Id},
	  {_,DisplayName},
	  {_,Mail}
	 ]}=Json,

	{reply, 
		#user{
			id=Id,
			displayName=binary_to_list(DisplayName),
			mail=binary_to_list(Mail)  
		}
	, Tab};
handle_call({user2json,User}, _From, Tab) ->
	{reply, 
		{[
	  		{<<"id">>,User#user.id},
	  		{<<"displayName">>,list_to_binary(User#user.displayName)},
	  		{<<"mail">>,list_to_binary(User#user.mail)}
	 	]}
	, Tab};




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


	