-module(event_fsm).
-behaviour(gen_fsm).

%% public API
-export([start/1, start_link/1, 
				select_date/2, 
				deselect_date/2, 
				reject/2,
				stop/2]).
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4,
			% custom state names
			open/2]).

-include("records.hrl").


-type event() :: #event {}.
-type user() :: #user {}.


%%% PUBLIC API
start(Event=#event{}) ->
	gen_fsm:start(?MODULE, Event, []).
 
start_link(Event=#event{}) ->
	gen_fsm:start_link(?MODULE, Event, []).

%%% EVENTS
select_date(OwnPid,{User,Day_ts}) ->
	gen_fsm:send_event(OwnPid,{select_date,User,Day_ts}).

deselect_date(OwnPid,{User,Day_ts}) ->
	gen_fsm:send_event(OwnPid,{deselect_date,User,Day_ts}).

reject(OwnPid,{User}) ->
	gen_fsm:send_event(OwnPid,{reject,User}).

%% stop the event.
stop(OwnPid,cancel) ->
    gen_fsm:send_all_state_event(OwnPid, cancel).

%%% GEN_FSM API

%% Set Event FSM to open state
-spec init(Event::event()) -> {ok, open, event()}.
init(Event=#event{}) ->
	{ok, open, Event}.

%%% STATE CALLBACKS
-spec open({select_date, User::user(), Day_ts::non_neg_integer()}, Event::event()) ->  {next_state,open,event()}.
open({select_date,User=#user{},Day_ts},Event=#event{}) ->
	ModEvent=event:add_user_to_event(Event,User,Day_ts),
	notice(ModEvent,"Add User To Event",[Day_ts]),
	{next_state,open,ModEvent};

open({deselect_date,User=#user{},Day_ts},Event=#event{}) ->
	%%remove user from day
	ModEvent=event:remove_user_from_event(Event,User,Day_ts),
	notice(ModEvent,"Remove User From Event",[Day_ts]),
	{next_state,open,ModEvent};

open({reject,User=#user{}},Event=#event{}) ->
	%%remove user from event
	ModEvent=event:reject_event(Event,User),
	notice(ModEvent,"Reject Event ",[User]),
	case lists:flatlength(ModEvent#event.contacts)>0 of
		false -> {stop, normal, Event};
		true -> {next_state,open,ModEvent}
	end;

open(Event, Data) ->
	unexpected(Event, open),
    {next_state, open, Data}.


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
    erlang:display(Str).
    %erlang:display(Event).
    %io:format("~n: "++Str++"~n", [Event|Args]).
