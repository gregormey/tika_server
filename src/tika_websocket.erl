-module(tika_websocket).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

%% custom
-export([sendEventsToRemote/2]).
-export([switchUserRemote/2]).

-include("records.hrl").

-type user() :: #user {}.


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    erlang:display("New Web Socket"),
    gproc:reg({p, l, wsbroadcast}), 
     %%set timeout to 5 min 
    {ok, Req, undefined_state,300000}. 

%%client calls
websocket_handle({text, Msg}, Req, State) ->
    erlang:display("GET Message:"),
    erlang:display(binary_to_list(Msg)),
    Response = handle_events(Msg),
    case Response of
        true -> {ok, Req, State};
        _ -> {reply, {text, << Response/binary >>}, Req, State}
    end;
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

%% server calls

websocket_info({_PID,Data}, Req, State) ->
    {reply, {text, jiffy:encode(Data)}, Req, State, hibernate};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%% Remote Interface
-spec sendEventsToRemote(user(),list()) -> true.
sendEventsToRemote(User, Events) ->
    erlang:display('Send events To'),
    erlang:display(User),
    case Events of
        not_found ->true;
        _ -> Response=format_client_response("updateEvents",[tika_event:event2json(X) || X <- Events ]),
            erlang:display(Events),
            erlang:display("Send Data:"),
            erlang:display(binary_to_list(Response)),
            {_PID, _Data}=gproc:send({p, l, {websocket,User#user.id}}, {self(), << Response/binary >>}),
            true
    end. 
-spec switchUserRemote(user(),user()) -> true.
switchUserRemote(User,NewUser) ->
    Response=format_client_response("switchUser",tika_user:user2json(NewUser)),
    {_PID, _Data}=gproc:send({p, l, {websocket,User#user.id}}, {self(), << Response/binary >>}).

%% events
%% Proxy for possible events
-spec handle_events(binary()) -> true | any().
handle_events(Msg) ->
    erlang:display(jiffy:decode(Msg)),
    case jiffy:decode(Msg) of
        {[{<< "Connect" >> , User}]} -> connectUser(User);
        {[{<< "UpdateUser" >> , User}]} -> updateUser(User);
        {[{<< "UpdateToken" >> , User}]} -> updateToken(User);
        {[{<< "CreateEvent" >> , Event}]} -> createEvent(Event);
        {[{<< "UpdateEvent" >> , Event}]} -> updateEvent(Event);
        {[{<< "User" >> , User},{<< "RefuseEvent" >> , Event}]} -> refuseEvent(User,Event);
        {[{<< "User" >> , User},{<< "Event" >> , Event},{<< "ConfirmDate" >> , DateTs}]} -> confirmDate(User,Event,DateTs);
        {[{<< "User" >> , User},{<< "Event" >> , Event},{<< "DeconfirmDate" >> , DateTs}]} -> deconfirmDate(User,Event,DateTs);
        {[{<< "FixEvent" >> , Event},{<< "Day" >> , Day}]} -> fixEvent(Event,Day);
            _ -> Msg
    end.

%% User Callbacks
updateUser(UserJson)->
   User=tika_user:json2user(UserJson),
   erlang:display("Update"),
   erlang:display(User#user.id),
   Pid=tika_process:id2pid(user,User#user.id),
   updateUserResponse(  User,
                        tika_user_fsm:update(Pid,{User#user.displayName,User#user.mail}),
                        tika_verification:load(mail,User#user.mail)).

updateToken(UserJson)->
   User=tika_user:json2user(UserJson),
   erlang:display("Update Token"),
   erlang:display(User#user.id),
   case is_number(User#user.id) of
        false -> true;
        true-> Pid=tika_process:id2pid(user,User#user.id),
               case Pid of
                   not_found -> false;
                   _ -> case tika_user_fsm:update_pushToken(Pid,{User#user.pushToken}) of 
                            ok -> true;
                            _ -> false 
                        end
                end
    end.
       

connectUser(null)-> 
    erlang:display("connect with null"), 
    User=tika_user:create(),
    gproc:reg({p, l, {websocket,User#user.id}}),
    format_client_response("updateUser",tika_user:user2json(User));
connectUser(UserJson)->
    erlang:display("connect with User"), 
    case tika_user:load(tika_user:json2user(UserJson)) of
            not_found -> connectUser(null);
            User->tika_user:load(tika_user:json2user(UserJson)),
                    erlang:display("New Websocket for"),
                    erlang:display(User#user.id),
                    gproc:reg({p, l, {websocket,User#user.id}}),
                    updateEventsMessage(User,"updateEvents")
    end.

%% Event Callbacks
createEvent(EventJson)->
    Event=tika_event:create(tika_event:json2event(EventJson)),
    Pid=tika_process:id2pid(event,Event#event.id),
    ok=tika_event_fsm:invite(Pid),
    true.

updateEvent(EventJson)->
    Event=tika_event:json2event(EventJson),
    Pid=tika_process:id2pid(event,Event#event.id),
    ok=tika_event_fsm:update(Pid,{Event#event.title,Event#event.description}),
    ok=tika_event_fsm:update_dates(Pid,{Event#event.dates}),
    ok=tika_event_fsm:update_contacts(Pid,{Event#event.contacts}),
    true.

refuseEvent(UserJson,EventJson)->
    User=tika_user:json2user(UserJson),
    Event=tika_event:json2event(EventJson),
    Pid=tika_process:id2pid(event,Event#event.id),
    ok=tika_event_fsm:reject(Pid,{User}),
    true.    

confirmDate(UserJson,EventJson,Day_ts)->
    User=tika_user:json2user(UserJson),
    Event=tika_event:json2event(EventJson),
    Pid=tika_process:id2pid(event,Event#event.id),
    ok=tika_event_fsm:confirm_date(Pid,{User,Day_ts}),
    true.

deconfirmDate(UserJson,EventJson,Day_ts)->
    User=tika_user:json2user(UserJson),
    Event=tika_event:json2event(EventJson),
    Pid=tika_process:id2pid(event,Event#event.id),
    ok=tika_event_fsm:deconfirm_date(Pid,{User,Day_ts}),
    true.

fixEvent(EventJson,DayJson)->
    Day = tika_event:json2day(DayJson),
    Event=tika_event:json2event(EventJson),
    Pid=tika_process:id2pid(event,Event#event.id),
    ok=tika_event_fsm:fix(Pid,{Day}),
    true.

%%% INTERNAL
-spec format_client_response(string(),any()) -> tuple().
format_client_response(Event,Data)->
    jiffy:encode({[
        {<< "event" >>,list_to_binary(Event)},
        {<< "data" >> , Data}
    ]}).

updateEventsMessage(User,Message)->
    case tika_event:findBy(user,User) of
        not_found -> format_client_response(Message,[]);
        Events -> format_client_response(Message,[tika_event:event2json(X) || X <- Events ])
    end.

updateUserResponse(User,UpdateResult, _) when UpdateResult == ok ->
     updateEventsMessage(User,"registerUser");
updateUserResponse(User,UpdateResult,Verification) when UpdateResult == user_exists,Verification#verification.verified=/=undefined->
     tika_verification:remove(mail, User#user.mail), 
     updateEventsMessage(User,"registerUser");
updateUserResponse(User,UpdateResult, Verification) when UpdateResult == user_exists, Verification==not_found ->
    tika_verification:create_verification(User#user.mail),
    format_client_response("registerUser",{[{<<"msg">>,<<"user_exists">>}]});
updateUserResponse(User,UpdateResult, Verification) when UpdateResult == user_exists, Verification#verification.verified==undefined ->
    tika_verification:create_verification(User#user.mail), 
    format_client_response("registerUser",{[{<<"msg">>,<<"user_exists">>}]}).




