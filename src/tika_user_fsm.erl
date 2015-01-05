-module(tika_user_fsm).
-behaviour(gen_fsm).

%% public API
-export([start/1, start_link/1, 
			invite/2,
            register/1,
            stop/2
		]).
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4,
			% custom state names
			created/2,
			invited/2,
			registered/2]).

-include("records.hrl").


-type event() :: #event {}.
-type user() :: #user {}.


%%% PUBLIC API
start(User=#user{}) ->
	gen_fsm:start(?MODULE, User, []).
 
start_link(User=#user{}) ->
	gen_fsm:start_link(?MODULE, User, []).

%%% EVENTS
invite(OwnPid,{Event=#event{}}) -> 
	gen_fsm:send_event(OwnPid,{invite, Event}).
register(OwnPid) -> 
    gen_fsm:send_event(OwnPid,register).

%%% GEN_FSM API

%% Set Event FSM to open state
-spec init(User::user()) -> {ok, created, user()}.
init(User=#user{}) ->
	{ok, created, User}.

%%% STATE CALLBACKS
-spec created({invite,Event::event()}, User::user()) ->  {next_state,invited,user()}.
created({invite,Event=#event{}},User=#user{}) ->
	tika_user:invite(User,Event),
	{next_state,invited,User};

created(register,User=#user{}) ->
    {next_state,registered,User};

created(Event, Data) ->
	unexpected(Event, created),
    {next_state, created, Data}.

invited(register,User=#user{}) ->
    {next_state,registered,User};

invited(Event, Data) ->
	unexpected(Event, invited),
    {next_state, created, Data}.
   
registered(Event, Data) ->
	unexpected(Event, invited),
    {next_state, created, Data}.

%% stop the user fsm.
stop(OwnPid,cancel) ->
    gen_fsm:send_all_state_event(OwnPid, cancel).
	
%% This cancel event has been sent by the event owner
%% stop whatever we're doing and shut down!
handle_event(cancel, _StateName, Event=#event{}) ->
    {stop, normal, Event};
handle_event(Event, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

%% Note: DO NOT reply to unexpected calls. Let the call-maker crash!
handle_sync_event(Event, _From, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

%% can be used for down event
handle_info(Info, StateName, Data) ->
    unexpected(Info, StateName),
    {next_state, StateName, Data}.


code_change(_OldVsn, StateName, Data, _Extra) ->
 {ok, StateName, Data}.

%% Event over.
terminate(normal, ready, Event=#event{}) ->
    notice(Event, "FSM leaving.", []);
terminate(_Reason, _StateName, _StateData) ->
    ok.


%%% PRIVATE FUNCTIONS

 %% Unexpected allows to log unexpected messages
unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in state ~p~n",
              [self(), Msg, State]).

%% Send players a notice. This could be messages to their clients
%% but for our purposes, outputting to the shell is enough.
notice(Event=#event{}, Str, Args) ->
    Str,
    Args,
    Event,
    erlang:display(Str).


