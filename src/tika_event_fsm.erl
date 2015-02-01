-module(tika_event_fsm).
-behaviour(gen_fsm).

%% public API
-export([start/1, start_link/1, 
				confirm_date/2, 
				deconfirm_date/2, 
				reject/2,
				edit/2,
				fix/2,
				over/1,
				stop/2]).
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4,
			% custom state names
			open/2,
			fixed/2]).

-include("records.hrl").


-type event() :: #event {}.
-type user() :: #user {}.


%%% PUBLIC API
start(Event=#event{}) ->
	gen_fsm:start(?MODULE, Event, []).
 
start_link(Event=#event{}) ->
	gen_fsm:start_link(?MODULE, Event, []).

%%% EVENTS
confirm_date(OwnPid,{User,Day_ts}) ->
	gen_fsm:send_event(OwnPid,{confirm_date,User,Day_ts}).

deconfirm_date(OwnPid,{User,Day_ts}) ->
	gen_fsm:send_event(OwnPid,{deconfirm_date,User,Day_ts}).

reject(OwnPid,{User}) ->
	gen_fsm:send_event(OwnPid,{reject,User}).

edit(OwnPid,{title,Title}) ->
	gen_fsm:send_event(OwnPid,{edit,title,Title});
edit(OwnPid,{description,Description}) ->
	gen_fsm:send_event(OwnPid,{edit,description,Description});
edit(OwnPid,{addContact,User=#user{}}) ->
	gen_fsm:send_event(OwnPid,{edit,addContact,User});
edit(OwnPid,{removeContact,User=#user{}}) ->
	gen_fsm:send_event(OwnPid,{edit,removeContact,User});
edit(OwnPid,{addDate,Day=#day{}}) ->
	gen_fsm:send_event(OwnPid,{edit,addDate,Day});
edit(OwnPid,{removeDate,Day=#day{}}) ->
	gen_fsm:send_event(OwnPid,{edit,removeDate,Day}).


fix(OwnPid,{Day}) ->
	gen_fsm:send_event(OwnPid,{fix,Day}).

over(OwnPid) ->
	gen_fsm:send_event(OwnPid,over).

%% stop the event.
stop(OwnPid,cancel) ->
    gen_fsm:send_all_state_event(OwnPid, cancel).

%%% GEN_FSM API

%% Set Event FSM to open state
-spec init(Event::event()) -> {ok, open, event()}.
init(Event=#event{}) ->
	{ok, open, Event}.

%%% STATE CALLBACKS
-spec open({confirm_date, User::user(), Day_ts::non_neg_integer()}, Event::event()) ->  {next_state,open,event()}.
open({confirm_date,User=#user{},Day_ts},Event=#event{}) ->
	ModEvent=tika_event:add_user_to_event(Event,User,Day_ts),
	notice(ModEvent,"Add User To Event",[Day_ts]),
	{next_state,open,ModEvent};

open({deconfirm_date,User=#user{},Day_ts},Event=#event{}) ->
	%%remove user from day
	ModEvent=tika_event:remove_user_from_event(Event,User,Day_ts),
	notice(ModEvent,"Remove User From Event",[Day_ts]),
	{next_state,open,ModEvent};

open({reject,User=#user{}},Event=#event{}) ->
	reject_event(User,Event);

open({edit,title,Title},Event=#event{}) ->
	ModEvent=tika_event:edit(Event,{title,Title}),
	notice(ModEvent,"Edit Event Title",[Title]),
	{next_state,open,ModEvent};

open({edit,description,Description},Event=#event{}) ->
	ModEvent=tika_event:edit(Event,{description,Description}),
	notice(ModEvent,"Edit Event Description",[Description]),
	{next_state,open,ModEvent};

open({edit,addContact,User=#user{}},Event=#event{}) ->
	ModEvent=tika_event:edit(Event,{addContact,User}),
	notice(ModEvent,"Edit Event add Contact",[User]),
	{next_state,open,ModEvent};

open({edit,removeContact,User=#user{}},Event=#event{}) ->
	ModEvent=tika_event:edit(Event,{removeContact,User}),
	notice(ModEvent,"Edit Event remove Contact",[User]),
	{next_state,open,ModEvent};

open({edit,addDate,Day=#day{}},Event=#event{}) ->
	ModEvent=tika_event:edit(Event,{addDate,Day}),
	notice(ModEvent,"Edit Event add Date",[Day]),
	{next_state,open,ModEvent};

open({edit,removeDate,Day=#day{}},Event=#event{}) ->
	ModEvent=tika_event:edit(Event,{removeDate,Day}),
	notice(ModEvent,"Edit Event remove Date",[Day]),
	{next_state,open,ModEvent};

open({fix,Day=#day{}},Event=#event{}) ->
	ModEvent=tika_event:fix(Event,Day),
	notice(ModEvent,"Event Fixed",[Day]),
	{next_state,fixed,ModEvent};

open(Event, Data) ->
	unexpected(Event, open),
    {next_state, open, Data}.

fixed({reject,User=#user{}},Event=#event{}) ->
	reject_event(User,Event);

fixed(over, Event=#event{}) ->
	notice(Event,"Event is Over",[]),
    {stop, normal, Event};

fixed(Event, Data) ->
	unexpected(Event, fixed),
    {next_state, fixed, Data}.


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
reject_event(User=#user{},Event=#event{}) ->
	ModEvent=tika_event:reject_event(Event,User),
	case lists:flatlength(ModEvent#event.contacts)>0 of
		false ->
			notice(ModEvent,"Stop Event-> All User Rejected",[User]),
			{stop, normal, Event};
		true -> 
			notice(ModEvent,"User Rejected",[User]),
			{next_state,open,ModEvent}
	end.

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
    %erlang:display(Str).
    Str.
