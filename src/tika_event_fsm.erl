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
				update/2,
				update_dates/2,
				update_contacts/2,
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

update(OwnPid,{Title,Description}) ->
	gen_fsm:send_event(OwnPid,{update,Title,Description}).

update_dates(OwnPid,{Dates}) ->
	gen_fsm:sync_send_event(OwnPid,{update_dates,Dates}).

update_contacts(OwnPid,{Contacts}) ->
	gen_fsm:sync_send_event(OwnPid,{update_contacts,Contacts}).


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
	Dates = Event#event.dates,
	Fun = (fun(Day=#day{})->
				case Day#day.timestamp of
					Day_ts -> 
							Guests = Day#day.guests,
							Day#day{guests = lists:delete(User,Guests)};
					_ -> Day
				end
			end),
	ModEvent=tika_event:update(Event#event{dates = lists:map(Fun,Dates)}),
	update_user_events(ModEvent#event.contacts),
	{next_state,open,ModEvent};

open({update,Title,Description},Event=#event{}) ->
	ModEvent=tika_event:update(Event#event{title=Title,description=Description}),
	update_user_events(ModEvent#event.contacts),
	{next_state,open,ModEvent};


open({reject,User=#user{}},Event=#event{}) ->
	reject_event(User,Event,open);


open({fix,Day=#day{}},Event=#event{}) ->
	ModEvent=tika_event:update(Event#event{appointment=Day}),
	update_user_events(ModEvent#event.contacts),
	{next_state,fixed,ModEvent};



open(Event, Data) ->
	unexpected(Event, open),
    {next_state, open, Data}.

open(invite,_From,Event=#event{contacts=Contacts})->
	{reply,ok,open,invite_user(Event,Contacts)};

open({update_dates,Dates},_From,Event=#event{}) ->
	NewDates = [NewDate || NewDate <- Dates, false==date_of_event(Event#event.dates,NewDate#day.timestamp)],
    KeepDates = [KeepDate || KeepDate <- Dates, date_of_event(Event#event.dates,KeepDate#day.timestamp)],
	ModEvent=tika_event:update(Event#event{dates=merge_event_dates(Event#event.dates,KeepDates,NewDates)}),
	update_user_events(ModEvent#event.contacts),
	{reply,ok,open,ModEvent};


open({update_contacts,Contacts},_From,Event=#event{}) ->
	NewContacts = [NewContact || NewContact <- Contacts, false==member_of_event(Event#event.contacts,NewContact)],
    invite_user(Event,NewContacts),

    KeepContacts = [KeepContact || KeepContact <- Contacts, member_of_event(Event#event.contacts,KeepContact)],
   
    RemoveContacts = [RemoveContact || RemoveContact <- Event#event.contacts, false==member_of_event(KeepContacts,RemoveContact)],
    ModEvent=tika_event:update(Event#event{
    			contacts=lists:merge(KeepContacts,NewContacts),
    			dates=remove_members_from_dates(RemoveContacts,Event#event.dates)
    		}),
	update_user_events(ModEvent#event.contacts),
	{reply,ok,open,ModEvent};

open(Event, _From, Data) ->
	unexpected(Event, open),
    {next_state, open, Data}.


fixed({fix,Day=#day{}},Event=#event{}) ->
	ModEvent=tika_event:update(Event#event{appointment=Day}),
	update_user_events(ModEvent#event.contacts),
	{next_state,fixed,ModEvent};

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

invite_user(Event=#event{creator=Creator},[User|T]) when User#user.mail =/= Creator#user.mail ->
	InviteUser = case tika_user:load(mail,User#user.mail) of 
					not_found -> tika_user:create(User);
					FoundUser -> FoundUser
				end,
	Pid=tika_process:id2pid(user,InviteUser#user.id),
	ok=tika_user_fsm:invite(Pid,Event),
	invite_user(Event,T);
invite_user(Event=#event{creator=Creator},[User|T]) when User#user.mail == Creator#user.mail-> 
	tika_websocket:sendEventsToRemote(User,tika_event:findBy(user,User)),
	invite_user(Event,T);
invite_user(Event,[])-> 
	Event.

update_user_events([]) -> ok;
update_user_events([User|T]) -> 
	tika_websocket:sendEventsToRemote(tika_user:load(mail,User#user.mail),tika_event:findBy(user,User)),
	update_user_events(T).


reject_event(User=#user{},Event=#event{},State) ->
	ModEvent=tika_event:update(Event#event{
			contacts = remove_contact(Event#event.contacts,User),
			dates = remove_members_from_dates([User],Event#event.dates)
	}),
	case lists:flatlength(ModEvent#event.contacts)>0 of
		false -> ok=tika_process:unreg(event,Event),
				{stop, normal, Event};
		true -> {next_state,State,ModEvent}
	end.

date_of_event([],_Ts)->
	false;
date_of_event([Date|T],Ts)->
	DateDay=tika_database:msToDate(Date#day.timestamp),
	IsDay=tika_database:msToDate(Ts),
	case IsDay==DateDay of
		true -> true;
		false -> date_of_event(T,Ts)
	end.

find_date([],_Ts) ->
	not_found;
find_date([Date|T],Ts) ->
	DateDay=tika_database:msToDate(Date#day.timestamp),
	IsDay=tika_database:msToDate(Ts),
	case IsDay==DateDay of
		true -> Date;
		false -> find_date(T,Ts)
	end.

member_of_event([],_Member)->
	false;
member_of_event([User|T],Member) ->
case Member#user.mail==User#user.mail of
		true -> true;
		false -> member_of_event(T,Member)
	end.

remove_contact(Guests,Contact)->
	[Guest || Guest<-Guests , Guest#user.mail=/=Contact#user.mail].

remove_members_from_dates([],Dates) ->
	Dates;
remove_members_from_dates([Contact|T],Dates) ->
	remove_members_from_dates(T,[Day#day{guests=remove_contact(Day#day.guests,Contact)} 
								|| Day <- Dates]).

merge_event_dates(Dates,Keep,New) ->
	lists:merge([find_date(Dates,Day#day.timestamp)||Day<-Keep],New).


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




