-module(tika_process).
-behaviour(gen_server).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%custom interfaces
-export([reg/2]).
-export([unreg/2]).

%% default interfaces
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).

-include("records.hrl").

-type process_event() :: #process_event {}.
-type process_user() :: #process_user {}.

%% Interfaces
-spec start() -> {ok, pid()} | {error, any()}.
start()-> gen_server:start_link({local,?MODULE},?MODULE,[], []).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec reg(atom(),non_neg_integer()) -> tuple().
reg(Record,Id) ->
	gen_server:call(?MODULE,{reg,Record,Id}).

-spec unreg(atom(),non_neg_integer()) -> tuple().
unreg(Record,Id) ->
	gen_server:call(?MODULE,{unreg,Record,Id}).

-spec stop() -> ok.
stop()-> gen_server:call(?MODULE, stop).

%% gen_server
init([]) ->
	mnesia:wait_for_tables([process_user,process_event],20000),
    {ok, ?MODULE}.

handle_call({reg,user,Id}, _From, Tab) ->
	{reply, 
		case tika_database:find(id,process_user,Id) of
			not_found -> {ok, Pid} = tika_user_fsm:start_link(),
						tika_database:write(process_user,#process_user{id=Id,pid=Pid});
			Result -> Result
		end
	, Tab};
handle_call({reg,event,Id}, _From, Tab) ->
	{reply, 
		case tika_database:find(id,process_event,Id) of
			not_found -> {ok, Pid} = tika_event_fsm:start_link(),
						tika_database:write(process_event,#process_event{id=Id,pid=Pid});
			Result -> Result
		end
	, Tab};
handle_call({unreg,user,Id}, _From, Tab) ->
	{reply, tika_database:delete(process_user,tika_database:find(id,process_user,Id)), Tab};
handle_call({unreg,event,Id}, _From, Tab) ->
	{reply, tika_database:delete(process_event,tika_database:find(id,process_event,Id)), Tab};

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