-module(tika_database).

-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").

%% interfaces to database
-export([install/0]).
-export([install/1]).
-export([id/1]).
-export([find/1]).
-export([find/2]).
-export([write/2]).


%% record to generate unique ids
-record( unique_ids, {type, id} ).

%% interfaces
-spec install() -> 'stopped' | {'error',_}.
install() ->
	tika:set_dbPath(), 
	create_tables().

%% set up schema for test enviroment
-spec install(test) -> 'stopped' | {'error',_}.
install(test)->
	create_tables().


%% next id for given table
-spec id(atom()) -> non_neg_integer().
id(Table)->
	mnesia:dirty_update_counter(unique_ids, Table, 1).


%% find specific records from table
-spec find(atom(),fun()) -> record() |  not_found.
find(Table,Filter)->
	case do(qlc:q([X || X <- mnesia:table(Table), 
							Filter(X)]))  of
		[] -> not_found;
		Results -> Results
	end.

%%get all records from database
-spec find(atom(),fun()) -> tuple() |  no_rows.
find(Table)->
	case do(qlc:q([X || X <- mnesia:table(Table)]))  of
		[] -> no_rows;
		Results -> Results
	end.


%% Update a player by given row
-spec write(atom(), tuple() -> tuple().
write(Table,Record)->
	Fw = fun() ->
			mnesia:write(Record)
		end,
	[Table,Id|_] = Record,
	Ff = fun(R) ->
		[Table,Id2|_]=tuple_to_list(R),
		Id==Id2,
	end,
	mnesia:transaction(Fw),
	find(Table,Ff).

%%% Private Functions

%% Mnesia query function
do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val. 

-spec create_tables() -> 'stopped' | {'error',_}.
create_tables()->
	%% delete schema in case another schema is running on this node
	ok=mnesia:delete_schema([node()]),
	ok=mnesia:create_schema([node()]),
	ok=mnesia:start(), 
	
	%% id table
	{atomic,ok}=mnesia:create_table(unique_ids, [{attributes, record_info(fields, unique_ids)},{disc_copies,[node()]}]),
	
	%% process table
	{atomic,ok}=mnesia:create_table(process_ids, [{attributes, record_info(fields, process_ids)},{ram_copies,[node()]}]),
	

	%% system models
	{atomic,ok}=mnesia:create_table(user,[{attributes,record_info(fields,user)},{disc_copies,[node()]}]),
	{atomic,ok}=mnesia:create_table(event,[{attributes,record_info(fields,event)},{disc_copies,[node()]}]),
	mnesia:stop(). 