-module(tika_event_fsm).
-behaviour(gen_fsm).

%% public API
-export([start/1, start_link/1,
				invite/1, 
				confirm_date/2, 
				deconfirm_date/2, 
				reject/2,
				fix/2,
				over/1,
				stop/2]).
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4,
			% custom state names
			open/2,
			open/3,
			fixed/2]).

-include("records.hrl").


-type event() :: #event {}.



%%% PUBLIC API
start(Event=#event{}) ->
	gen_fsm:start(?MODULE, Event, []).
 
start_link(Event=#event{}) ->
	gen_fsm:start_link(?MODULE, Event, []).

%%% EVENTS
invite(OwnPid) ->
	gen_fsm:sync_send_event(OwnPid,invite).

confirm_date(OwnPid,{User,Day_ts}) ->
	gen_fsm:send_event(OwnPid,{confirm_date,User,Day_ts}).

deconfirm_date(OwnPid,{User,Day_ts}) ->
	gen_fsm:send_event(OwnPid,{deconfirm_date,User,Day_ts}).

reject(OwnPid,{User}) ->
	gen_fsm:send_event(OwnPid,{reject,User}).



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

open({confirm_date,User=#user{},Day_ts},Event=#event{}) ->
	Dates = Event#event.dates,
	Fun = (fun(Day=#day{})->
				case Day#day.timestamp of
					Day_ts -> 
							Guests = Day#day.guests,
							Day#day{guests = lists:append(Guests,[User])};
					_ -> Day
				end
			end),
	ModEvent=tika_event:update(Event#event{dates = lists:map(Fun,Dates)}),
	update_user_events(ModEvent#event.contacts),
	{next_state,open,ModEvent};

open({deconfirm_date,User=#user{},Day_ts},Event=#event{}) ->
	%%remove user from day
	ModEvent=tika_event:remove_user_from_event(Event,User,Day_ts),
	notice(ModEvent,"Remove User From Event",[Day_ts]),
	{next_state,open,ModEvent};

open({reject,User=#user{}},Event=#event{}) ->
	reject_event(User,Event,open);


open({fix,Day=#day{}},Event=#event{}) ->
	ModEvent=tika_event:fix(Event,Day),
	notice(ModEvent,"Event Fixed",[Day]),
	{next_state,fixed,ModEvent};

open(Event, Data) ->
	unexpected(Event, open),
    {next_state, open, Data}.


open(invite,_From,Event=#event{contacts=Contacts})->
	{reply,ok,open,invite_user(Event,Contacts)};

open(Event, _From, Data) ->
	unexpected(Event, open),
    {next_state, open, Data}.


fixed({reject,User=#user{}},Event=#event{}) ->
	reject_event(User,Event,fixed);

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
    ok=tika_process:unreg(event,Event),
    notice(Event, "FSM leaving.", []);  
terminate(_Reason, _StateName, Event=#event{}) ->
    ok=tika_process:unreg(event,Event),
    ok.

%%% PRIVATE FUNCTIONS
invite_user(Event,[])-> 
	Event;
invite_user(Event,[User|T])->
	InviteUser = case tika_user:load(mail,User#user.mail) of 
					not_found -> tika_user:create(User);
					FoundUser -> FoundUser
				end,
	Pid=tika_process:id2pid(user,InviteUser#user.id),
	ok=tika_user_fsm:invite(Pid,Event),
	invite_user(Event,T).

update_user_events([]) -> ok;
update_user_events([User|T]) -> 
	tika_websocket:sendEventsToRemote(User,tika_event:findBy(user,User)),
	update_user_events(T).


reject_event(User=#user{},Event=#event{},State) ->
	Contacts=Event#event.contacts,
	ModEvent=tika_event:update(Event#event{contacts = lists:delete(User,Contacts)}),
	case lists:flatlength(ModEvent#event.contacts)>0 of
		false -> ok=tika_process:unreg(event,Event),
				{stop, normal, Event};
		true -> {next_state,State,ModEvent}
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
