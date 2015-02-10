-module(tika_websocket).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

%% custom
-export([sendEventToRemote/2]).

-include("records.hrl").

-type user() :: #user {}.
-type event() :: #event {}.

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
-spec sendEventToRemote(user(),event()) -> true.
sendEventToRemote(User,Event) ->
    erlang:display("Send Data To:"++binary_to_list(User#user.id)),
    {_PID, Data}=gproc:send({websocket,User#user.id}, {self(), 
                            format_client_response("Invite",tika_event:event2json(Event))}),
    true.

%% events
%% Proxy for possible events
-spec handle_events(binary()) -> true | any().
handle_events(Msg) ->
    erlang:display(jiffy:decode(Msg)),
    case jiffy:decode(Msg) of
        {[{<< "Connect" >> , User}]} -> connectUser(User);
        {[{<< "UpdateUser" >> , User}]} -> updateUser(User);
        {[{<< "CreateEvent" >> , Event}]} -> createEvent(Event);
            _ -> Msg
    end.

%% User Callbacks
updateUser(UserJson)->
   User=tika_user:json2user(UserJson),
   Pid=tika_process:id2pid(user,User#user.id),
   case tika_user_fsm:update(Pid,{User#user.displayName,User#user.mail}) of 
        ok -> format_client_response("registerUser",{[{<<"msg">>,<<"ok">>}]});
        user_exists -> format_client_response("registerUser",{[{<<"msg">>,<<"user_exists">>}]})
   end.

connectUser(null)-> 
    User=tika_user:create(),
    gproc:reg({p, l, {websocket,User#user.id}}),
    format_client_response("updateUser",tika_user:user2json(User));
connectUser(UserJson)->
    User=tika_user:load(tika_user:json2user(UserJson)),
    gproc:reg({p, l, {websocket,User#user.id}}),
    true.

%% Event Callbacks
createEvent(EventJson)->
    tika_event:create(tika_event:json2event(EventJson)),
    true.


%%% INTERNAL
-spec format_client_response(string(),any()) -> tuple().
format_client_response(Event,Data)->
    jiffy:encode({[
        {<< "event" >>,list_to_binary(Event)},
        {<< "data" >> , Data}
    ]}).

    


