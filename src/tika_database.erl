-module(tika_database).
-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% interfaces to database
-export([install/0]).
-export([install/1]).
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).
-export([create/2]).

%% record to generate unique ids
-record( unique_ids, {type, id} ).

-type event() :: #event {}.
-type user() :: #user {}.

%% interfaces
-spec install() -> 'stopped' | {'error',_}.
install() ->
	tika:set_dbPath(), 
	create_tables().

%% set up schema for test enviroment
-spec install(test) -> 'stopped' | {'error',_}.
install(test)->
	create_tables().

-spec create_tables() -> 'stopped' | {'error',_}.
create_tables()->
	%% delete schema in case another schema is running on this node
	ok=mnesia:delete_schema([node()]),
	ok=mnesia:create_schema([node()]),
	ok=mnesia:start(), 
	%% id table
	{atomic,ok}=mnesia:create_table(unique_ids, [{attributes, record_info(fields, unique_ids)},{disc_copies,[node()]}]),
	
	%% system models
	{atomic,ok}=mnesia:create_table(user,[{attributes,record_info(fields,user)},{disc_copies,[node()]}]),
	{atomic,ok}=mnesia:create_table(event,[{attributes,record_info(fields,event)},{disc_copies,[node()]}]),
	mnesia:stop(). 


-spec start() -> {ok, pid()} | {error, any()}.
start()-> gen_server:start_link({local,?MODULE},?MODULE,[], []).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop()-> gen_server:call(?MODULE, stop).

-spec create(atom(),event()|user()) -> event()|user().
create(Table,Record) -> gen_server:call(?MODULE,{create,Table,Record}).

%% Internal functions

%% Mnesia query function
do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val. 

%% next id for given table
-spec id(atom()) -> non_neg_integer().
id(Table)->
	mnesia:dirty_update_counter(unique_ids, Table, 1).

%% find single player from database
-spec find(user,Id::non_neg_integer()) -> user() |  not_a_user.
find(user,Id)->
	case do(qlc:q([X || X <- mnesia:table(user), 
							X#user.id == Id]))  of
		[] -> not_a_user;
		[User] -> User
	end.

%% Update a player by given row
-spec write(user, user()) -> user().
write(user,User)->
	F = fun() ->
			mnesia:write(User)
		end,
	mnesia:transaction(F),
	find(user,User#user.id).


%% gen_server
init([]) ->
	mnesia:start(),
	mnesia:wait_for_tables([unique_ids,event,user],20000),
    {ok, ?MODULE}.

%% call handler to create user with a new id 
handle_call({create,user,Record}, _From, Tab) ->
	User = Record#user{id=id(user)},
	{reply, write(user,User) , Tab};

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


