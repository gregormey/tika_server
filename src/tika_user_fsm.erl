-module(tika_user_fsm).
-behaviour(gen_fsm).

%% public API
-export([start/1, start_link/1, 
            update/2,
            update_pushToken/2,
            statename/1,
            invite/2,
            user/1,
            stop/2
		]).
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4,
			% custom state names
			created/2,
            created/3,
			invited/2,
            invited/3,
			registered/2,
            registered/3]).

-include("records.hrl").


-type user() :: #user {}.


%%% PUBLIC API
start(User=#user{}) ->
	gen_fsm:start(?MODULE, User, []).
 
start_link(User=#user{}) ->
	gen_fsm:start_link(?MODULE, User, []).

%%% EVENTS
update(OwnPid,{DisplayName,Mail}) -> 
    gen_fsm:sync_send_event(OwnPid,{update,DisplayName,Mail}).

update_pushToken(OwnPid,{Token}) -> 
    gen_fsm:sync_send_event(OwnPid,{update_pushToken,Token}).

invite(OwnPid,Event)->
    gen_fsm:sync_send_event(OwnPid,{invite,Event}).    

%%%% INFO EVENTS
statename(OwnPid) ->
    gen_fsm:sync_send_all_state_event(OwnPid,which_statename).
user(OwnPid) ->
    gen_fsm:sync_send_all_state_event(OwnPid,which_user).


%%% GEN_FSM API

%% Set Event FSM to open state
-spec init(User::user()) -> {ok, created, user()}.
init(User=#user{}) ->
    %%set state
	{ok, 
        case User#user.registered of
            undefined -> case User#user.invited of
                                undefined -> created;
                                _ -> invited
                        end;
                    _ -> registered
        end, 
    User}.

%%% STATE CALLBACKS

created({update,DisplayName,Mail},_From,User=#user{}) ->
    UpdateUser= fun(MyUser) ->
       NewUser=tika_user:update(MyUser#user{
                                    displayName=DisplayName,
                                    mail=Mail,
                                    registered=tika_database:unixTS()
                            }),
        NewUser 
    end,
    case tika_user:load(mail,Mail) of 
        not_found -> {reply,ok,registered,UpdateUser(User)};
        FoundUser -> case FoundUser#user.id==User#user.id of
                        %% TDO: clean it up and use functions
                        true -> {reply,ok,registered,UpdateUser(User)};
                        false -> case FoundUser of
                                    #user{registered=Registered,invited=Invited} when Registered==undefined, Invited=/=undefined-> 
                                                        tika_database:delete(user, User),
                                                        tika_websocket:switchUserRemote(User,FoundUser), 
                                                        {reply,ok,registered,UpdateUser(FoundUser)};
                                    _ -> {reply,user_exists,created,User}
                                end
                    end
    end;

created({update_pushToken,Token},_From,User=#user{}) ->
    {reply,ok,created,
        tika_user:update(User#user{pushToken=Token})
    };

created({invite,#event{title=EventTitle, creator=Creator}},_From, User=#user{mail=Mail,displayName=UserName}) ->
    tika_mail:send_invite(Mail,Creator#user.displayName,UserName,EventTitle),
    InvitedUser=tika_user:update(User#user{invited=tika_database:unixTS()}),
    {reply,ok,invited,InvitedUser};    

created(Event, _From,Data) ->
    unexpected(Event, created),
    {next_state, created, Data}.


created(Event, Data) ->
    unexpected(Event, created),
    {next_state, created, Data}.

invited({update,DisplayName,Mail},_From,User=#user{}) ->
      NewUser = tika_user:update(User#user{
                                    displayName=DisplayName,
                                    mail=Mail,
                                    registered=tika_database:unixTS()
                            }),
      {reply,ok,registered,NewUser};

invited({invite,#event{title=EventTitle, creator=Creator}}, _From, User=#user{mail=Mail,displayName=UserName}) ->
    tika_mail:send_invite(Mail,Creator#user.displayName, UserName,EventTitle),
    {reply,ok,invited,User};  

invited(Event, _From,Data) ->
    unexpected(Event, created),
    {next_state, created, Data}.


invited(Event, Data) ->
    unexpected(Event, invited),
    {next_state, invited, Data}.

registered({update,DisplayName,Mail},_From,User=#user{}) ->
    UpdateUser= fun() ->
       NewUser=tika_user:update(User#user{
                                    displayName=DisplayName,
                                    mail=Mail
                            }),
        NewUser 
    end,
    case tika_user:load(mail,Mail) of 
        not_found -> {reply,ok,registered,UpdateUser()};
        FoundUser -> case FoundUser#user.id==User#user.id of
                        true -> {reply,ok,registered,UpdateUser()};
                        false -> {reply,user_exists,registered,User}
                    end
    end;

registered({update_pushToken,Token},_From,User=#user{}) ->
    {reply,ok,registered,
        tika_user:update(User#user{pushToken=Token})
    };

registered({invite,Event=#event{}},_From, User=#user{}) ->
    tika_push:send_invite(User, Event), 
    tika_websocket:sendEventsToRemote(User,tika_event:findBy(user,User)),
    {reply,ok,registered,User}; 


registered(Event, _From, Data) ->
    unexpected(Event, invited),
    {next_state, registered, Data}.


registered(Event, Data) ->
    unexpected(Event, invited),
    {next_state, registered, Data}.

%% stop the user fsm.
stop(OwnPid,cancel) ->
    gen_fsm:send_all_state_event(OwnPid, cancel).
	
%% Intorsepctions
handle_event(which_statename, StateName, User=#user{}) ->
    {reply, StateName, StateName, User};
handle_event(which_user, StateName, User=#user{}) ->
    {reply,User,StateName,User};

%% This cancel event has been sent by the event owner
%% stop whatever we're doing and shut down!

handle_event(cancel, _StateName, User=#user{}) ->
    {stop, normal, User};
handle_event(Event, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

%% Intorsepctions
handle_sync_event(which_statename, _From, StateName, User=#user{}) ->
    {reply, StateName, StateName, User};
handle_sync_event(which_user, _From, StateName, User=#user{}) ->
    {reply,User,StateName,User};

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
terminate(normal, ready, User=#user{}) ->
    ok=tika_process:unreg(user,User),
    notice(User, "FSM leaving.", []);  
terminate(_Reason, _StateName, User=#user{}) ->
    ok=tika_process:unreg(user,User),
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


