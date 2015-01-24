-module(tika_database).

-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").

%% interfaces to database
-export([install/0]).
-export([install/1]).
-export([find/1]).
-export([find/2]).
-export([find/3]).
-export([write/2]).
-export([create/2]).
-export([delete/2]).


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

-spec find(id,atom(),non_neg_integer()) -> record() |  not_found.
find(id,Table,Id)->
	find(Table,filterById(Id)).

%%get all records from database
-spec find(atom()) -> tuple() |  no_rows.
find(Table)->
	case do(qlc:q([X || X <- mnesia:table(Table)]))  of
		[] -> no_rows;
		Results -> Results
	end.


%% Update a player by given row
-spec write(atom(), tuple()) -> tuple().
write(Table,Record)->
	Fw = fun() ->
			mnesia:write(Record)
		end,
	[Table,Id|_] = Record,
	mnesia:transaction(Fw),
	find(Table,filterById(Id)).

-spec create(atom(),tuple()) -> tuple().
create(Table,Record) ->
	Id=id(Table),
	[Table,_|T] = Record,
	write(Table,[Table,Id|T]).

-spec delete(atom(),tuple()) -> any().
delete(Table,Record) ->
	[Table,Id|_] = Record,
	case find(Table,filterById(Id))of
		not_found -> not_found;
		Result -> {atomic, Val} = mnesia:transaction(
					fun () -> mnesia:delete_object(Result) end
					),
					Val
	end.

%%% Private Functions
filterById(Id)->
	fun(R) ->
		[Table,Id2|_]=tuple_to_list(R),
		Id==Id2
	end.


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
	
	%% process table - ram only
	{atomic,ok}=mnesia:create_table(process_user, [{attributes, record_info(fields, process_user)},{ram_copies,[node()]}]),
	{atomic,ok}=mnesia:create_table(process_event, [{attributes, record_info(fields, process_event)},{ram_copies,[node()]}]),
	

	%% system models
	{atomic,ok}=mnesia:create_table(user,[{attributes,record_info(fields,user)},{disc_copies,[node()]}]),
	{atomic,ok}=mnesia:create_table(event,[{attributes,record_info(fields,event)},{disc_copies,[node()]}]),
	mnesia:stop(). 