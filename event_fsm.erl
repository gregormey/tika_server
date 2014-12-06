-module(event_fsm).
-behaviour(gen_fsm).

%% public API
-export([start/1, start_link/1, open_event/2, accept_date/1,
		select_date/2, reject_event/2]).
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4,
			% custom state names
			open/2, fixed/3, closed/2]).

-include("records.hrl").


-type event() :: #event {}.


%%% PUBLIC API
start(Name) ->
	gen_fsm:start(?MODULE, [Name], []).
 
start_link(Name) ->
	gen_fsm:start_link(?MODULE, [Name], []).

%% Ser Event FSM to open state
-spec init(Event::event()) -> {ok, open, event()}
init(Event=#event{}) ->
	{ok, open, Event}.

open({select_date,User=#user{},Day=#day{}},Event=#event{}) ->
	%%add user to day
	{}

