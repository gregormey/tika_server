-module(tika_rest).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([get/3]).
-export([is_authenticated/3]).
-export([terminate/4]).



init(_Route, _Req, State) ->
    {ok, State}.

authenticated(Req, State) ->
	User=list_to_binary(tika_config:get_value(config,[config,backend_user], "username")),
	Passwd=list_to_binary(tika_config:get_value(config,[config,backend_passwd], "passwd")),
	case leptus_req:auth(Req, basic) of
		{User,Passwd} -> {true, State};
		_ -> {false, [{<<"WWW-Authenticate">>, <<"Basic">>}], <<"Auth Required">>, State}
	end.

is_authenticated("/event/", Req, State) ->
	authenticated(Req, State);

is_authenticated("/user/", Req, State) ->
	authenticated(Req, State);

is_authenticated(_, Req, State) ->
	{true, State}.

get("/verify/:code", Req, State) ->
	Code=binary_to_list(leptus_req:param(Req,code)),
	case tika_verification:verify(Code) of 
   		not_found-> {404, {json, [{<<"Msg">>,<<"Code not found">>}]}, State};
   		ok -> {200, {json, [{<<"Msg">>,<<"Code verified">>}]}, State};
   		_ -> {500, {json, [{<<"Msg">>,<<"Internal Server Error">>}]}, State}
   	end;

get("/event/", _Req, State) ->
	{200, {json, 
			[tika_event:event2json(inc_dates,X) || X <- tika_event:list() ] 
	}, State};

get("/user/", _Req, State) ->
	{200, {json, 
			[tika_user:user2json(inc_dates,X) || X <- tika_user:list() ] 
	}, State}.

terminate(_Reason, _Route, _Req, _State) ->
    ok.