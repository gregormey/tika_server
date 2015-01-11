-module(tika_websocket).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-include("records.hrl").

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
    erlang:display(binary_to_list(Response)),
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

%% events
%% Proxy for possible events
-spec handle_events(binary()) -> true | any().
handle_events(Msg) ->
    erlang:display(jiffy:decode(Msg)),
    case jiffy:decode(Msg) of
        {[{<< "Connect" >> , User}]} -> connectUser(User);
        [{<< "To" >> , To } , {<< "Data" >> , Data }] -> sendObjectsToRemote(To, Data);
        _ -> Msg
    end.

connectUser(null)-> 
    User=tika_user:create(),
    gproc:reg({p, l, User#user.id}),
    erlang:display(User),
    jiffy:encode({[{<< "updateUser" >> , null}]}).

%% Register Websocket connection by User Hash
-spec registerHash(binary()) -> true.
registerHash(UserHash) ->
    erlang:display("Register User:"++binary_to_list(UserHash)),
    gproc:reg({p, l, UserHash}).

-spec sendObjectsToRemote(binary(),binary()) -> true.
sendObjectsToRemote(To,Data) ->
    erlang:display("Send Data To:"++binary_to_list(To)),
    {_PID, Data}=gproc:send({p, l, To}, {self(), Data}),
    true.
    


