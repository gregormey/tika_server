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
-export([id2pid/2]).


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

-spec reg(atom(),user()|event()) -> tuple().
reg(Record,Object) ->
	gen_server:call(?MODULE,{reg,Record,Object}).

-spec unreg(atom(),non_neg_integer()) -> tuple().
unreg(Record,Object) ->
	gen_server:call(?MODULE,{unreg,Record,Object}).

-spec id2pid(atom(),non_neg_integer()) -> pid().
id2pid(Record,Id) ->
	gen_server:call(?MODULE,{id2pid,Record,Id}).

-spec stop() -> ok.
stop()-> gen_server:call(?MODULE, stop).

%% gen_server
init([]) ->
	mnesia:wait_for_tables([process_user,process_event],20000),
    {ok, ?MODULE}.

handle_call({reg,user,User}, _From, Tab) ->
	{reply, 
		process(user,User#user.id)
	, Tab};
handle_call({reg,event,Event}, _From, Tab) ->
	{reply, 
		process(event,Event#event.id)
	, Tab};
handle_call({unreg,user,User}, _From, Tab) ->
	[Process]=tika_database:find(id,process_user,User#user.id),
	{reply, tika_database:delete(process_user,Process), Tab};
handle_call({unreg,event,Event}, _From, Tab) ->
	[Process]=tika_database:find(id,process_event,Event#event.id),
	{reply, tika_database:delete(process_event,Process), Tab};

handle_call({id2pid,user,Id}, _From, Tab) ->
	Process=process(user,Id),
	{reply, Process#process_user.pid, Tab};

handle_call({id2pid,event,Id}, _From, Tab) ->
	Process=process(event,Id),
	{reply, Process#process_event.pid, Tab};

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


%%% internals
process(user,Id) ->
	case tika_database:find(id,process_user,Id) of
			not_found ->
						case tika_database:find(id,user,Id) of
							not_found -> user_not_found;
							[User] -> {ok, Pid} = tika_user_fsm:start_link(User),
									 [Process]=tika_database:write(process_user,#process_user{id=User#user.id,pid=Pid}),
									 Process
						end;
			[Process] -> Process
	end;

process(event,Id) ->
	case tika_database:find(id,process_event,Id) of
			not_found ->
						case tika_database:find(id,event,Id) of
							not_found -> event_not_found;
							[Event] -> {ok, Pid} = tika_event_fsm:start_link(Event),
										[Process]=tika_database:write(process_event,#process_event{id=Event#event.id,pid=Pid}),
										Process
						end;
			[Process] -> Process
	end.

